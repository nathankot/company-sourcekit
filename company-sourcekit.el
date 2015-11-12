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

(defcustom company-sourcekit-use-yasnippet t
  "Should Yasnippet be used for completion expansion"
  :type 'boolean)

(defun company-sourcekit--fetch (prefix)
  "Use sourcekitten to get a list of completion candidates.
PREFIX is the file offset passed to sourcekitten."
  (message "Retrieving from sourcekitten using PREFIX %s" prefix)
  (let ((tmpfile (make-temp-file "sourcekitten"))
        (offset (point)))
    (write-region (point-min) (point-max) tmpfile)
    (with-temp-buffer
      (message "Calling: sourcekitten complete --file %s --offset %d" tmpfile offset)
      (call-process company-sourcekit-sourcekitten-executable nil (current-buffer) nil
                    "complete" "--file" tmpfile "--offset" (number-to-string offset))
      (setq return-json (buffer-substring-no-properties (point-min) (point-max)))
      (append (mapcar
               (lambda (l)
                 (let* ((counter 0)
                        (v (cdr (assoc 'sourcetext l)))
                        (n (cdr (assoc 'descriptionKey l)))
                        (f (replace-regexp-in-string "<#T##\\(.*?\\)#>"
                                                     (lambda (blk)
                                                       (save-match-data
                                                         (string-match "<#T##\\(.*?\\)#>" blk)
                                                         (setq counter (+ 1 counter))
                                                         (format "${%i:%s}" counter (car (split-string (match-string 1 blk) "#")))
                                                         )) v)))
                   (propertize n 'yas-template f)))
               (json-read-from-string return-json)) nil))))

(declare-function yas-expand-snippet "yasnippet")
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
    (post-completion
     (when company-sourcekit-use-yasnippet
       (let ((template (get-text-property 0 'yas-template prefix)))
         (yas-expand-snippet template
                             (- (point) (length prefix))
                             (point)))))
    (sorted t)))

(provide 'company-sourcekit)
;;; company-sourcekit.el ends here

