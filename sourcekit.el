;;; sourcekit.el --- Library to interact with sourcekittendaemon  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Nathan Kot

;; Author: Nathan Kot <nk@nathankot.com>
;; URL: https://github.com/nathankot/company-sourcekit
;; Keywords: tools, processes
;; Version: 0.2.0
;; Package-Requires: ((emacs "24.3") (dash "2.18.0") (request "0.2.0"))

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

;; sourcekit.el is a library that interacts with sourcekittendaemon in order
;; to communicate with SourceKit. Given a Xcode project in the directory tree,
;; It provides functions to spin up/down daemons, discover existing daemons,
;; and send queries to them either synchronous or asynchronously.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'request)

(defgroup sourcekit nil
  "Library to interface with sourcekitten daemon"
  :group 'sourcekit)

(defcustom sourcekit-available-ports
  '(8081 8082 8083 8084 8085 8086 8087 8088 8089 8090)
  "The list of ports that sourcekittendaemon is permitted to listen on."
  :type 'integer
  :group 'sourcekit)

(defcustom sourcekit-sourcekittendaemon-executable
  (executable-find "sourcekittendaemon")
  "Location of sourcekittendaemon."
  :type 'file
  :group 'sourcekit)

(defcustom sourcekit-verbose nil
  "Should log with verbosity to the messages buffer."
  :type 'boolean
  :group 'sourcekit)

(defvar sourcekit-start-daemon-lock nil
  "Ensures that there is at most one daemon start attempt.")

(defun sourcekit-stop ()
  "Stop any known sourcekit processes and queries for this buffer."
  (interactive)
  (-when-let (p (get-process "sourcekit-query")) (delete-process p))
  (-when-let (p (get-process (format "sourcekit-daemon:%s" (sourcekit-project)))) (delete-process p)))

(defvar-local sourcekit-project 'unknown)
(defun sourcekit-project ()
  (when (eq sourcekit-project 'unknown)
    (setq sourcekit-project
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
  sourcekit-project)

(defvar-local sourcekit-last-daemon-port nil)
(defun sourcekit-with-daemon-for-project (project cb)
  "Ensuring that a daemon for PROJECT exists, call the CB.
CB is called with the port as the first argument, nil if the daemon cannot be created."
  ;; Look for an existing port we can try
  (let ((port (or
                sourcekit-last-daemon-port
                (-first
                  (lambda (p)
                    (string-equal project (sourcekit-query-sync p "/project")))
                  sourcekit-available-ports))))
    (if port
      ;; If we already have a port, give it to the callback
      (progn
        (setq sourcekit-last-daemon-port port)
        (when sourcekit-verbose (message "[sourcekit] trying daemon on port: %d" port))
        (funcall cb port))

      ;; Otherwise try to spin up a new daemon
      (if (not sourcekit-start-daemon-lock)
        (-when-let* ((unused-port (-first (lambda (p) (not (eq "OK" (sourcekit-query-sync p "/ping"))))
                                    sourcekit-available-ports))
                      (_ (progn (setq sourcekit-start-daemon-lock t) t))
                      (process (start-process
                                 (format "sourcekit-daemon:%s" project)
                                 (when sourcekit-verbose "*sourcekit-daemon-process*")
                                 sourcekit-sourcekittendaemon-executable
                                 "start"
                                 "--port" (number-to-string unused-port)
                                 "--project" project)))

          (when sourcekit-verbose
            (message
              (concat "[sourcekit] could not find existing port, "
                "attempting to start daemon on port: %d")
              unused-port))

          (set-process-sentinel process
            (lambda (proc status)
              ;; On any startup failures, delete the lock
              (unless (string-match "run" status)
                (message "[sourcekit] daemon startup failure: %s" status)
                (setq sourcekit-start-daemon-lock nil)
                (setq sourcekit-last-daemon-port nil)
                (funcall cb nil))))

          (set-process-filter process
            (lambda (proc str)
              (when sourcekit-verbose (message "[sourcekit] process message: %s" str))
              (-when-let (found-port (save-match-data (and (string-match "0.0.0.0:\\([0-9][0-9]*\\)" str) (match-string 1 str))))
                (when sourcekit-verbose
                  (message "[sourcekit] daemon listening on port %d" (string-to-number found-port)))
                (set-process-sentinel process nil)
                (set-process-filter process nil)
                (setq sourcekit-last-daemon-port (string-to-number found-port))
                (setq sourcekit-start-daemon-lock nil)
                ;; Now that we have a new daemon, re-run this function again
                (sourcekit-with-daemon-for-project project cb)))))

        (when sourcekit-verbose
          (message "[sourcekit] skipping daemon startup due to existing lock"))
        (funcall cb nil)))))

(defun sourcekit-query-sync (port path)
  "Run a query against the sourcekit daemon on PORT and PATH synchronously.
Passes ARGS as additional arguments to curl.
It returns either the response stdout or nil for error.
This does not reset the cached daemon port, even on failures.
This differs from sourcekit-query in that it does not consider error responses as failures either, hence the 'lax'"
  (let ((response (request (format "http://localhost:%d%s" port path)
                    :sync t :parser 'buffer-string :timeout 2)))
    (if (request-response-error-thrown response) nil
      (request-response-data response))))

(defun sourcekit-query (port path headers cb)
  "Run a query against the sourcekit daemon on PORT and PATH, passing ARGS as additional arguments to curl.
CB will be given the response JSON on a successful request. If a query ever fails, it will reset the cached daemon port."
  (let ((url (format "http://localhost:%d%s" port path)))
    (when sourcekit-verbose
      (message "[sourcekit] making request to %s" url)
      (message "[sourcekit] with headers %S" headers))
    (request url
      :parser 'json-read
      :headers headers
      :success (cl-function
                 (lambda (&key data &allow-other-keys)
                   (when sourcekit-verbose
                     (message "[sourcekit] got query response"))
                   (funcall cb data)))
      :error (cl-function
               ;; When exiting with an error, try get a new daemon
               (lambda (&rest args &key error-thrown &allow-other-keys)
                 (when sourcekit-verbose (message "[sourcekit] got error %S" error-thrown))
                 (setq sourcekit-last-daemon-port nil))))))

(provide 'sourcekit)
;;; sourcekit.el ends here
