;;; company-sourcekit --- company-mode completion back-end for sourcekit

;;; Commentary:

(require 'company)
(require 'cl-lib)
(require 'json)

;;; Code:

(defgroup company-sourcekit nil
  "Completion backend for swift projects"
  :group 'company)

(defcustom company-sourcekit-sourcekitten-executable (executable-find "sourcekitten")
  "Location of sourcekitten executable."
    :type 'file)

(defvar-local company-sourcekit--workspace 'unknown)
(defun company-sourcekit--workspace ()
  "Retrieve the xcode workspace for the current file by searching up the directory hierarchy."
  (if (eq company-sourcekit--workspace 'unknown)
    (setq company-sourcekit--workspace
      (let ((mFile
             (locate-dominating-file buffer-file-name
               (lambda (parent)
                 (and
                   (not (null parent))
                   (file-directory-p parent)
                   (directory-files parent nil ".*?\\.xcworkspace"))))))
        (unless (null mFile)
          (expand-file-name mFile)))))
  company-sourcekit--workspace)

(defvar-local company-sourcekit--project 'unknown)
(defun company-sourcekit--project ()
  "Retrieve the xcode project for the current file by searching up the directory hierarchy."
  (if (eq company-sourcekit--project 'unknown)
    (setq company-sourcekit--project
      (let ((mFile
             (locate-dominating-file buffer-file-name
               (lambda (parent)
                 (and
                   (not (null parent))
                   (file-directory-p parent)
                   (directory-files parent nil ".*?\\.xcodeproj"))))))
        (unless (null mFile)
          (expand-file-name mFile)))))
  company-sourcekit--project)

(defun company-sourcekit--fetch (prefix)
  "Use sourcekitten to get a list of completion candidates.
PREFIX is the file offset passed to sourcekitten."
  (message "Retrieving from sourcekitten using PREFIX %s" prefix)
  (let ((tmpfile (make-temp-file "sourcekitten"))
         (offset (point)))
    (write-region (point-min) (point-max) tmpfile)
    (with-temp-buffer
      (let ((workspace-or-project (if (company-sourcekit--workspace)
                                    (concat "-workspace " (company-sourcekit--workspace))
                                    (if (company-sourcekit--project)
                                      (concat "-project " (company-sourcekit--project)) ""))))
        (message "Calling process with --file %s --offset %d %s" tmpfile offset workspace-or-project)
        (call-process company-sourcekit-sourcekitten-executable nil (current-buffer) nil
          "complete" "--file" tmpfile "--offset" offset workspace-or-project)
        (append (mapcar (lambda (l) (assoc 'descriptionKey l))
          (json-read-from-string (buffer-substring-no-properties (point-min) (point-max)))) nil)))))

(defun company-sourcekit (command &optional prefix &rest ignored)
  "Company backend for swift using sourcekitten, listening to the COMMAND.
PREFIX is taken as the current point in the buffer
IGNORED ignores the rest of the arguments"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-sourcekit))
    (prefix (and (eq major-mode 'swift-mode)
                 (not (company-in-string-or-comment))
                 (company-grab-symbol-cons "\\." 1)))
    (candidates (company-sourcekit--fetch prefix))
    (sorted t)))

(provide 'company-sourcekit)
;;; company-sourcekit.el ends here
