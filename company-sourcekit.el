;;; company-sourcekit.el --- company-mode completion backend for SourceKit  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Nathan Kot

;; Author: Nathan Kot <nk@nathankot.com>
;; URL: https://github.com/nathankot/company-sourcekit
;; Keywords: abbrev
;; Version: 0.1.4
;; Package-Requires: ((emacs "24.3") (company "0.8.12") (dash "2.12.1") (dash-functional "1.2.0") (sourcekit "0.1.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A company-mode backend for swift projects. It communicates with SourceKit
;; via sourcekittendaemon in order to obtain completions for Xcode projects.

;;; Code:

(require 'company)
(require 'cl-lib)
(require 'json)
(require 'dash)
(require 'dash-functional)
(require 'sourcekit)

(defgroup company-sourcekit nil
  "Completion backend that uses sourcekit"
  :group 'company)

(defcustom company-sourcekit-use-yasnippet
  (fboundp 'yas-minor-mode)
  "Should Yasnippet be used for completion expansion."
  :type 'boolean
  :group 'company-sourcekit)

(defcustom company-sourcekit-verbose nil
  "Should log with verbosity to the messages buffer."
  :type 'boolean
  :group 'company-sourcekit)

;;;###autoload
(defun company-sourcekit (command &optional arg &rest ignored)
  "Company backend for swift using sourcekitten."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-sourcekit))
    (init (progn
            (unless sourcekit-sourcekittendaemon-executable
              (error "[company-sourcekit] sourcekittendaemon not found in PATH"))
            (if (eq (sourcekit-project) 'unknown)
              (error "[company-sourcekit] *.xcodeproj not found in directory tree"))))
    (sorted t)
    (prefix (company-sourcekit--prefix))
    (candidates (cons :async (lambda (cb) (company-sourcekit--candidates arg cb))))
    (meta (company-sourcekit--meta arg))
    (annotation (company-sourcekit--annotation arg))
    (post-completion (company-sourcekit--post-completion arg))))

;;; Private:

(defun company-sourcekit--prefix ()
  "In our case, the prefix acts as a cache key for company-mode.
It never actually gets sent to the completion engine."
  (and
    (eq major-mode 'swift-mode)
    (not (company-in-string-or-comment))
    (or
      (-when-let* ((x (company-grab-symbol-cons "import ")) (_ (listp x))) x)
      (-if-let (x (company-grab "\w*(\\(.*?\\)" 1 (line-beginning-position))) (cons x t))
      (company-grab-symbol-cons "\\."))))

(defun company-sourcekit--meta (candidate)
  "Gets the meta for the completion candidate."
  (get-text-property 0 'description candidate))

(defun company-sourcekit--annotation (candidate)
  "Gets the type of the completion candidate."
  (format " %s" (get-text-property 0 'type candidate)))

(defun company-sourcekit--candidates (prefix callback)
  "Use sourcekitten to get a list of completion candidates."
  (sourcekit-with-daemon-for-project (sourcekit-project)
    (lambda (port)
      (if (not port) (funcall callback nil)
        (let* (
                (tmpfile (make-temp-file "sourcekitten"))
                (offset (- (point) (length prefix))))
          (write-region (point-min) (point-max) tmpfile)
          (when company-sourcekit-verbose
            (message "[company-sourcekit] prefix: `%s`, file: %s, offset: %d" prefix tmpfile offset))
          ;; Make HTTP request to the sourcekittendaemon, asynchronously
          (sourcekit-query port
            "/complete"
            (company-sourcekit--make-sentinel callback)
            "-H" (format "X-Offset: %d" offset)
            "-H" (format "X-Path: %s" tmpfile)))))))

(defun company-sourcekit--make-sentinel (callback)
  "The handler for process output."
  (lambda (proc status output)
    (when company-sourcekit-verbose
      (message "[company-sourcekit] query got status: %s" status))
    (when (or (string-match "exit" status) (string-match "finished" status))
      (if (eq 0 (process-exit-status proc))
        (condition-case nil
          (let ((completions (company-sourcekit--process-json output)))
            (when company-sourcekit-verbose
              (message "[company-sourcekit] sending results to company"))
            (funcall callback completions))
          (error (funcall callback nil)))
        (company-sourcekit--handle-error status)))))

(defun company-sourcekit--process-json (return-json)
  "Given json returned from sourcekitten, turn it into a list compatible with company-mode"
  (append (mapcar
           (lambda (l)
             (let ((name (cdr (assoc 'name l)))
                   (desc (cdr (assoc 'descriptionKey l)))
                   (src (cdr (assoc 'sourcetext l)))
                   (type (cdr (assoc 'typeName l))))
               (propertize (company-sourcekit--normalize-source-text src)
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
      (yas-expand-snippet template (- (point) (length completed)) (point)))))

(defun company-sourcekit--normalize-source-text (sourcetext)
  "Make a more readable completion candidate out of one with placeholders."
  (replace-regexp-in-string
   "<#T##\\(.*?\\)#>"
   (lambda (str)
     ;; <#T##Int#> - No label, argument only
     (save-match-data
       (string-match "<#T##\\(.*?\\)#>" str)
       (format "%s" (car (split-string (match-string 1 str) "#")))))
   sourcetext))

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
