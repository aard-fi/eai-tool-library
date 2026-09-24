;;; eai-tool-library-emacs.el --- Simple and safe emacs information gathering tools -*- lexical-binding: t; -*-
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
;; This module contains some safe modules useful for gathering various data from
;; emacs. For potentially destructive tools as well as for elisp development
;; support see the `elisp' module.
;;
;;; Code:

(require 'eai-tool-library)

(defvar eai-tool-library-emacs-tools '()
  "The list of emacs related tools")
;; we skip the other two variables as this module should only contain safe tools

(defun eai-tool-library-emacs--describe-variable (var)
  "Return documentation for VAR."
  (eai-tool-library--debug-log (format "describe-variable %s" var))
  (let ((symbol (intern var)))
    (if (boundp symbol)
        (prin1-to-string (symbol-value symbol))
      (format "Variable %s is not bound. This means the variable doesn't exist. Stop. Reassess what you're trying to do, examine the situation, and continue. " var))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-emacs-tools
 :function #'eai-tool-library-emacs--describe-variable
 :name  "describe-variable"
 :description "Returns variable contents. After calling this tool, stop. Evaluate if the result helps. Then continue fulfilling user's request."
 :args (list '(:name "var"
                     :type string
                     :description "Variable name"))
 :category "emacs")

(defun eai-tool-library-emacs--describe-function (fun)
  "Return documentation for FUN."
  (eai-tool-library--debug-log (format "describe-function %s" fun))
  (let ((symbol (intern fun)))
    (if (fboundp symbol)
        (prin1-to-string (documentation symbol 'function))
      (format "Function %s is not defined." fun))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-emacs-tools
 :function #'eai-tool-library-emacs--describe-function
 :name  "describe-function"
 :description "Returns function description. After calling this tool, stop. Evaluate if the result helps. Then continue fulfilling user's request."
 :args (list '(:name "fun"
                     :type string
                     :description "Function name"
                     :optional t))
 :category "emacs")

(defun eai-tool-library-emacs--save-buffer-confirm (&optional buffer-name)
  "Return non-nil if save-buffer needs confirmation for BUFFER-NAME."
  (let ((buf (if buffer-name (get-buffer buffer-name) (current-buffer))))
    (eai-tool-library-write-confirm-p (when buf (buffer-file-name buf)))))

(defun eai-tool-library-emacs--save-buffer (&optional buffer-name)
  "Save BUFFER-NAME to its associated file.
BUFFER-NAME is a string or buffer object. If nil, use the current
buffer. Returns the file path saved, or an error message if the
buffer has no file association."
  (eai-tool-library--debug-log (format "save-buffer %s" buffer-name))
  (let ((buffer (if buffer-name
                    (get-buffer buffer-name)
                  (current-buffer))))
    (with-current-buffer buffer
      (if (not (buffer-file-name))
          (error "Buffer %s has no associated file; use write-file instead"
                 (buffer-name buffer))
        (let ((file (buffer-file-name)))
          (eai-tool-library-write-deny-check file)
          (save-buffer)
          (format "Saved %s to %s" (buffer-name buffer) file))))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-emacs-tools
 :function #'eai-tool-library-emacs--save-buffer
 :name  "save-buffer"
 :description "Save a buffer to its associated file. Returns the file path on success. Use this after creating or modifying a file buffer."
 :args (list '(:name "buffer-name"
                     :type string
                     :description "Name of the buffer to save. Uses current buffer if omitted."
                     :optional t))
 :category "emacs"
 :confirm #'eai-tool-library-emacs--save-buffer-confirm)

(provide 'eai-tool-library-emacs)
;;; eai-tool-library-emacs.el ends here
