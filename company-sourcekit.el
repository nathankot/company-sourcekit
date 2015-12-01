;;; -*- lexical-binding: t -*-
;;; company-sourcekit --- company-mode completion back-end for sourcekit
;;; Package-Requires: ((dash "2.12.1") (dash-functional "1.2.0")

;;; Commentary:

(require 'company)
(require 'cl-lib)
(require 'json)
(require 'dash)
(require 'dash-functional)

;;; Code:

(defgroup company-sourcekit nil
  "Completion backend that uses sourcekit"
  :group 'company)

(defcustom company-sourcekit-sourcekittendaemon-executable
  (executable-find "sourcekittendaemon")
  "Location of sourcekittendaemon."
  :type 'file)

(defcustom company-sourcekit-curl-executable
  (executable-find "curl")
  "Location of curl."
  :type 'file)

(defcustom company-sourcekit-port
  4766
  "The first port number that will be used for sourcekittendaemon.
Any additional daemons will use ports starting from this number in increments of 1"
  :type 'integer)

(defcustom company-sourcekit-default-configuration
  nil
  "The default configuration to build."
  :type 'string)

(defcustom company-sourcekit-use-yasnippet
  (fboundp 'yas-minor-mode)
  "Should Yasnippet be used for completion expansion."
  :type 'boolean)

(defcustom company-sourcekit-verbose nil
  "Should log to the messages buffer."
  :type 'boolean)

(defun company-sourcekit (command &optional arg &rest ignored)
  "Company backend for swift using sourcekitten.
COMMAND is the command to run for this backend
ARG are the arguments passed to the command
IGNORED are ignored"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-sourcekit))
    (init (unless company-sourcekit-sourcekittendaemon-executable
            (error "[company-sourcekit] sourcekittendaemon not found in PATH")))
    (sorted t)
    (prefix (company-sourcekit--prefix))
    (candidates (cons :async (lambda (cb) (company-sourcekit--candidates arg cb))))
    (meta (company-sourcekit--meta arg))
    (annotation (company-sourcekit--annotation arg))
    (post-completion (company-sourcekit--post-completion arg))))

;;; Private:

(defvar-local company-sourcekit--project 'unknown)
(defun company-sourcekit--project ()
  (when (eq company-sourcekit--project 'unknown)
    (setq company-sourcekit--project
      (let ((dir (if buffer-file-name
                   (file-name-directory buffer-file-name)
                   (expand-file-name default-directory)))
             (prev-dir nil)
             file)
        (while (not (or file (equal dir prev-dir)))
          (setq file (car (directory-files dir t ".xcodeproj\\'" t))
            prev-dir dir
            dir (file-name-directory (directory-file-name dir))))
        file)))
  company-sourcekit--project)

(defvar company-sourcekit--daemon nil)
(defun company-sourcekit--daemon-for (project)
  "Get or create a daemon for the given project.
Company-sourcekit only keeps one daemon instance running at any given time.
So if a daemon already exists for another project, it will be killed and overwritten."
  (if (and company-sourcekit--daemon (eq project (cdr (assoc 'project company-sourcekit--daemon))))
    company-sourcekit--daemon
    ;; Otherwise stop the currenct process and start a new one
    (-when-let ((d company-sourcekit--daemon)) (company-sourcekit--stop-daemon-process company-sourcekit--daemon))
    (let* (
            (port company-sourcekit-port)
            (daemon (list
                      (cons 'port port)
                      (cons 'project project)
                      (cons 'configuration company-sourcekit-default-configuration)
                      (cons 'target nil)
                      (cons 'process-name (concat "company-sourcekit-daemon:" project)))))
      (setq company-sourcekit--daemon daemon)
      daemon)))

(defun company-sourcekit--start-or-restart-daemon-process (daemon)
  "Start/restart the process for the given daemon object."
  (company-sourcekit--stop-daemon-process daemon)
  (company-sourcekit--start-daemon-process daemon))

(defun company-sourcekit--start-daemon-process (daemon)
  "Start the process of a daemon"
  (when company-sourcekit-verbose
      (message "[company-sourcekit] Starting daemon for: %s" (cdr (assoc 'project daemon))))
  (eval `(start-process
           ,(cdr (assoc 'process-name daemon))
           (when company-sourcekit-verbose "*sourcekit-daemon-process*")
           ,company-sourcekit-sourcekittendaemon-executable
           "start"
           "--port" ,(number-to-string (cdr (assoc 'port daemon)))
           "--project" ,(cdr (assoc 'project daemon))
           ,@(-when-let ((x (cdr (assoc 'target daemon)))) `("--target" ,x))
           ,@(-when-let ((x (cdr (assoc 'configuration daemon)))) `("--configuration" ,x)))))

(defun company-sourcekit--stop-daemon-process (daemon)
  "Stop the process of a daemon if it exists."
  (-when-let ((p (get-process (cdr (assoc ('process-name daemon))))))
    (when company-sourcekit-verbose
      (message "[company-sourcekit] Stopping daemon for: %s" (cdr (assoc 'project daemon))))
    (delete-process p)))

(defun company-sourcekit--prefix ()
  "In our case, the prefix acts as a cache key for company-mode.
It never actually gets sent to the completion engine."
  (and
    (eq major-mode 'swift-mode)
    (not (company-in-string-or-comment))
    (or
      (company-grab-symbol-cons "\\.")
      (company-grab-word))))

(defun company-sourcekit--meta (candidate)
  "Gets the meta for the completion candidate."
  (get-text-property 0 'description candidate))

(defun company-sourcekit--annotation (candidate)
  "Gets the type of the completion candidate."
  (format " %s" (get-text-property 0 'type candidate)))

(defun company-sourcekit--candidates (prefix callback)
  "Use sourcekitten to get a list of completion candidates."
  (let* (
          (tmpfile (make-temp-file "sourcekitten"))
          ;; What sort of completion are we doing?
          ;; SourceKit is strict about the offsets that we give it so our
          ;; final offset will depend on this.
          (offsetoffset
            (or
              ;; Properties or methods
              ;; Sourcekitten works well when the offset is a dot (.)
              ;; So lets give it the position of the last dot based on the prefix
              (and (eq ?. (char-before (- (point) (length prefix)))) (length prefix))
              ;; Import statements
              (and (eq (string-match "import \\w*" (thing-at-point 'line)) 0) (length prefix))
              ;; Words
              0))
          ;; SourceKit uses an offset starting @ 0, whereas emacs' starts at 1,
          ;; therefore subtract `point-min`
          (offset (- (point) offsetoffset (point-min)))
          (buf (get-buffer-create "*sourcekit-output*"))
          (daemon (company-sourcekit--daemon-for (company-sourcekit--project))))

    (company-sourcekit--start-or-restart-daemon-process daemon)
    (write-region (point-min) (point-max) tmpfile)
    (with-current-buffer buf (erase-buffer) (buffer-disable-undo))

    (when company-sourcekit-verbose
      (message "[company-sourcekit] prefix: `%s`, file: %s, offset: %d" prefix tmpfile offset))
    ;; Make HTTP request to the sourcekittendaemon, asynchronously
    (let* ((process (start-process
                      "company-sourcekit-query" buf company-sourcekit-curl-executable
                      "-f" ;; Exit code != 0 on error status responses
                      "-H" (format "X-Offset: %d" offset)
                      "-H" (format "X-Path: %s" tmpfile)
                      (format "http://localhost:%d/complete" (cdr (assoc 'port daemon))))))
      (set-process-sentinel process (company-sourcekit--sentinel buf callback)))))

(defun company-sourcekit--sentinel (buf callback)
  "The handler for process output."
  (lambda (proc status)
    (unless (string-match-p "hangup" status)
      (if (eq 0 (process-exit-status proc))
          (let ((completions
                 (with-current-buffer buf
                   (company-sourcekit--process-json
                    (buffer-substring-no-properties (point-min) (point-max))))))
            (when company-sourcekit-verbose
              (progn (message "[company-sourcekit] sending completion results:")
                     (prin1 completions)))
            (funcall callback completions))
        (company-sourcekit--handle-error status)))))

(defun company-sourcekit--process-json (return-json)
  "Given json returned from sourcekitten, turn it into a list compatible with company-mode"
  (append (mapcar
           (lambda (l)
             (let ((name (cdr (assoc 'name l)))
                   (desc (cdr (assoc 'descriptionKey l)))
                   (src (cdr (assoc 'sourcetext l)))
                   (type (cdr (assoc 'typeName l))))
               (propertize name
                           'sourcetext src
                           'description desc
                           'type type)))
           (json-read-from-string return-json)) nil))

(defun company-sourcekit--handle-error (status)
  (when company-sourcekit-verbose
    (message "[company-sourcekit] failed with status: %s" status)))

(declare-function yas-expand-snippet "yasnippet")
(defun company-sourcekit--post-completion (completed)
  "Post completion - expand yasnippet if necessary"
  (when company-sourcekit-use-yasnippet
    (when company-sourcekit-verbose (message "[company-sourcekit] expanding yasnippet template"))
    (let ((template (company-sourcekit--build-yasnippet (get-text-property 0 'sourcetext completed))))
      (when company-sourcekit-verbose (message "[company-sourcekit] %s" template))
      (yas-expand-snippet template (- (point) (length completed) 1) (point)))))

(defun company-sourcekit--build-yasnippet (sourcetext)
  "Build a yasnippet-compatible snippet from the given source text"
  (replace-regexp-in-string
   "<#T##\\(.*?\\)#>"
   (lambda (str)
     ;; <#T##Int#> - No label, argument only
     (save-match-data
       (string-match "<#T##\\(.*?\\)#>" str)
       (format "${%s}" (car (split-string (match-string 1 str) "#")))))
   sourcetext))

(provide 'company-sourcekit)
;;; company-sourcekit.el ends here
