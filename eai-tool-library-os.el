;;; eai-tool-library-os.el --- OS interaction tools -*- lexical-binding: t; -*-
;;
;; Author: Bernd Wachter
;;
;; Copyright (c) 2025 Bernd Wachter
;;
;; Keywords: tools
;;
;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details. http://www.gnu.org/copyleft/gpl.html
;;
;;; Commentary:
;;
;;; Code:

(require 'eai-tool-library)

(defvar eai-tool-library-os-tools '()
  "The list of OS related tools")

(defvar eai-tool-library-os-tools-maybe-safe '()
  "The list of OS related tools which may be destructive, but typically
the LLM behaves.")

(defvar eai-tool-library-os-tools-unsafe '()
  "The list of OS related tools which are not safe.")

(defvar eai-tool-library-os-category-name "OS"
  "The buffer category used for tool registration")

(defun eai-tool-library-os--run-shell (command)
  "Return output of SHELL COMMAND."
  (eai-tool-library--debug-log (format "shell %s" command))
  (with-temp-buffer
    (shell-command command (current-buffer) nil)
    (concat
     (buffer-string))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-os-tools-unsafe
 :function #'eai-tool-library-os--run-shell
 :name  "run-shell"
 :description "Executs a bash shell command and returns its output. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "command"
                     :type string
                     :description "The shell command to execute."))
 :category "OS"
 :confirm t)

(defun eai-tool-library-os--read-file-contents (filename)
  "Return contents of FILE."
  (eai-tool-library--debug-log (format "read-file-contents %s" filename))
  (with-temp-buffer
    (insert-file-contents (expand-file-name filename))
    (concat
     (buffer-string))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-os-tools-maybe-safe
 :function #'eai-tool-library-os--read-file-contents
 :name  "read-file-contents"
 :description "Reads and return the contents of a specified file. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "filename"
                     :type string
                     :description "The filename to read."))
 :category "OS")

(defun eai-tool-library-os--get-default-directory (&optional buffer-name)
  "Return the `default-directory' of BUFFER-NAME, or the current buffer if nil.
BUFFER-NAME is a string or buffer object. If nil, use the current buffer."
  (let ((buffer (if buffer-name
                    (get-buffer buffer-name)
                  (current-buffer))))
    (with-current-buffer buffer
      default-directory)))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-os-tools
 :function #'eai-tool-library-os--get-default-directory
 :name  "get-current-directory"
 :description "Return the default-directory of BUFFER-NAME, or the current buffer if nil."
 :args (list
        '(:name "buffer-name"
                :type string
                :optional t
                :description "Optional name of buffer to query; when omitted, use current buffer."))
 :category "OS")

(defun eai-tool-library-os--set-default-directory (directory &optional buffer-name)
  "Set `default-directory' to DIRECTORY for BUFFER-NAME, or the current buffer.

DIRECTORY is a string. BUFFER-NAME is a string or buffer object. If nil, use the
current buffer."
  (let ((buffer (if buffer-name
                    (get-buffer buffer-name)
                  (current-buffer))))
    (with-current-buffer buffer
      (setq default-directory (expand-file-name directory)))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-os-tools-maybe-safe
 :function #'eai-tool-library-os--set-default-directory
 :name  "set-current-directory"
 :description "Set default-directory to DIRECTORY for BUFFER-NAME, or the current buffer.."
 :args (list
        '(:name "directory"
                :type string
                :description "The new default directory to set.")
        '(:name "buffer-name"
                :type string
                :optional t
                :description "Optional name of buffer to modify; when omitted, use the current buffer."))
 :category "OS")

(defun eai-tool-library-os--write-file-confirm (filename _contents)
  "Return non-nil if write-file needs confirmation for FILENAME."
  (eai-tool-library-write-confirm-p (expand-file-name filename)))

(defun eai-tool-library-os--write-file (filename contents)
  "Write CONTENTS to FILENAME, creating parent directories if needed.
Returns the absolute path of the file written."
  (eai-tool-library--debug-log (format "write-file %s" filename))
  (let ((path (expand-file-name filename)))
    (eai-tool-library-write-deny-check path)
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert contents))
    (format "Wrote %d bytes to %s" (length contents) path)))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-os-tools-maybe-safe
 :function #'eai-tool-library-os--write-file
 :name  "write-file"
 :description "Write CONTENTS to FILENAME, creating parent directories if needed. Overwrites existing files."
 :args (list '(:name "filename"
                     :type string
                     :description "The file path to write to.")
             '(:name "contents"
                     :type string
                     :description "The text to write into the file."))
 :category "OS"
 :confirm #'eai-tool-library-os--write-file-confirm)

(provide 'eai-tool-library-os)
;;; eai-tool-library-os.el ends here
