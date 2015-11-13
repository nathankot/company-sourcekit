;;; -*- lexical-binding: t -*-
;;; company-sourcekit --- company-mode completion back-end for sourcekit

;;; Commentary:

(require 'company)
(require 'cl-lib)
(require 'json)

;;; Code:

(defgroup company-sourcekit nil
  "Completion backend that uses sourcekit"
  :group 'company)

(defcustom company-sourcekit-sourcekitten-executable
  (executable-find "sourcekitten")
  "Location of sourcekitten executable."
    :type 'file)

(defcustom company-sourcekit-use-yasnippet
  (fboundp 'yas-minor-mode)
  "Should Yasnippet be used for completion expansion"
  :type 'boolean)

(defcustom company-sourcekit-verbose nil
  "Should log to the messages buffer"
  :type 'boolean)

(defun company-sourcekit (command &optional arg &rest ignored)
  "Company backend for swift using sourcekitten"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-sourcekit))
    (sorted t)
    (no-cache t)
    (prefix (company-sourcekit--prefix))
    (candidates (cons :async (lambda (cb) (company-sourcekit--candidates arg cb))))
    (meta (company-sourcekit--meta arg))
    (annotation (company-sourcekit--annotation arg))
    (post-completion (company-sourcekit--post-completion arg))))

;;; Private:

(defun company-sourcekit--prefix ()
  (and (eq major-mode 'swift-mode)
       (not (company-in-string-or-comment))
       (company-grab-symbol-cons "\\." 1)))

(defun company-sourcekit--meta (candidate)
  "Returns the meta for the completion candidate"
  (get-text-property 0 'description candidate))

(defun company-sourcekit--annotation (candidate)
  "Returns the type of the completion candidate"
  (format " %s" (get-text-property 0 'type candidate)))

(defun company-sourcekit--candidates (prefix callback)
  "Use sourcekitten to get a list of completion candidates."
  (when company-sourcekit-verbose
    (message "[company-sourcekit] retrieving from sourcekitten using prefix: %s" prefix))
  (let ((tmpfile (make-temp-file "sourcekitten"))
        (offset (point)))
    ;; Use a temporary file as the source to sourcekitten
    (write-region (point-min) (point-max) tmpfile)
    (let ((buf (get-buffer-create "*sourcekit-output*"))
          (p (get-process "company-sourcekit")))
      ;; Clean up by killing the existing process and erasing the buffer (order is important!)
      (if p (progn
              (when company-sourcekit-verbose
                (message "[company-sourcekit] killing existing sourcekit process"))
              (delete-process p)))
      (when company-sourcekit-verbose
        (message "[company-sourcekit] erasing sourcekit output buffer"))
      (with-current-buffer buf (erase-buffer))
      ;; Run an async process and attach our output handler to it
      (let ((process (start-process "company-sourcekit" buf company-sourcekit-sourcekitten-executable
                                    "complete" "--file" tmpfile "--offset" (number-to-string offset))))
        (set-process-sentinel process (company-sourcekit--sentinel buf callback))))))

(defun company-sourcekit--sentinel (buf callback)
  "The handler for process output"
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
    (message "[company-sourcekit] sourcekitten failed with status: %s" status)))

(declare-function yas-expand-snippet "yasnippet")
(defun company-sourcekit--post-completion (completed)
  "Post completion - expand yasnippet if necessary"
  (when company-sourcekit-use-yasnippet
    (when company-sourcekit-verbose (message "[company-sourcekit] expanding yasnippet template"))
    (let ((template (company-sourcekit--build-yasnippet (get-text-property 0 'sourcetext completed))))
      (when company-sourcekit-verbose (message "[company-sourcekit] %s" template)
      (yas-expand-snippet template (- (point) (length completed)) (point))))))

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
