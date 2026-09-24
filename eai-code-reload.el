;;; eai-code-reload.el --- Reload uncommitted Lisp changes -*- lexical-binding: t; -*-
;;
;; Author: Bernd Wachter
;;
;; Copyright (c) 2026 Bernd Wachter
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
;; For long-running Emacs sessions where you iteratively develop and reload
;; code (especially useful with agentic workflows), this module provides:
;;
;; - `eai-code-reload'              — eval all eai-* .el files in the project
;;
;; - `eai-code-reload-uncommitted' — eval all top-level definitions from files
;;   with uncommitted changes.  Remembers which names were touched.
;;
;; - `eai-code-reload-revert' — revert touched names back to their last
;;   committed (HEAD) versions.  Names that did not exist in HEAD are unbound.
;;
;; - `eai-code-reload-status' — show what was touched and whether a committed
;;   version exists.
;;
;; The name registry is important: if an agent modifies a function but later
;; the change is abandoned (the function is no longer in the diff), a simple
;; "revert from diff" would miss it.  By tracking names explicitly, we can
;; always restore the committed state.
;;
;;; Code:

(require 'cl-lib)
(require 'project)

(defgroup eai-code-reload nil
  "Reload uncommitted Lisp changes."
  :group 'eai-code)

(defvar eai-code-reload--registry nil
  "Alist of (NAME . FILE) for definitions reloaded from uncommitted changes.
NAME is a symbol.  FILE is the path to the source file.
This is global across sessions; see `eai-code-reload-clear' to reset.")

(defvar eai-code-reload--definition-types
  '(defun defun* defsubst defmacro defvar defconst defcustom
          defalias defface define-minor-mode define-derived-mode
          cl-defun cl-defmacro cl-defstruct cl-defgeneric cl-defmethod
          defgeneric defmethod)
  "List of symbols that introduce top-level definitions we care about.")

(defvar eai-code-reload-files
  '("eai-code.el"
    "eai-code-agent.el"
    "eai-code-compact.el"
    "eai-code-error.el"
    "eai-code-gptel-stub.el"
    "eai-code-metrics.el"
    "eai-code-monitor.el"
    "eai-code-reload.el"
    "eai-tool-library.el"
    "eai-tool-library-bbdb.el"
    "eai-tool-library-buffer.el"
    "eai-tool-library-date-time.el"
    "eai-tool-library-elisp.el"
    "eai-tool-library-emacs.el"
    "eai-tool-library-gnus.el"
    "eai-tool-library-org.el"
    "eai-tool-library-os.el"
    "eai-tool-library-outline.el"
    "eai-tool-library-project.el"
    "eai-tool-library-search-and-replace.el"
    "eai-tool-library-url.el")
  "List of filenames (relative to project root) that `eai-code-reload' evaluates.
Add or remove files here to control what gets reloaded.")

(defun eai-code-reload--top-level-forms (file)
  "Return all top-level forms from FILE as a list of (NAME TYPE FORM-STRING).
NAME is the defined symbol, TYPE is the definer symbol,
FORM-STRING is the full sexp as a string.
Returns nil if the file is not readable or contains no definitions."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((forms nil)
            (case-fold-search nil))
        (goto-char (point-min))
        (condition-case _err
            (while (not (eobp))
              (let ((start (point))
                    (form (read (current-buffer))))
                (when (and (listp form)
                           (symbolp (car form))
                           (memq (car form) eai-code-reload--definition-types))
                  (let ((name (and (> (length form) 1)
                                   (if (eq (car form) 'cl-defstruct)
                                       (if (listp (cadr form))
                                           (cadr form)
                                         (cadr form))
                                     (cadr form)))))
                    (when (symbolp name)
                      (push (list name (car form)
                                  (buffer-substring-no-properties start (point)))
                            forms))))))
          ((end-of-file scan-error)
           nil))
        (nreverse forms)))))

(defun eai-code-reload--committed-forms (file)
  "Return top-level forms from FILE at HEAD, as (NAME TYPE FORM-STRING).
Uses `git show HEAD:FILE' to get the committed version.
Returns nil if FILE is not tracked or has no committed version."
  (let ((default-directory (or (when-let* ((proj (project-current)))
                                 (project-root proj))
                               default-directory)))
    (with-temp-buffer
      (when (zerop (call-process "git" nil t nil
                                 "show" (format "HEAD:%s"
                                                (file-relative-name file default-directory))))
        (let ((forms nil)
              (case-fold-search nil))
          (goto-char (point-min))
          (condition-case _err
              (while (not (eobp))
                (let ((start (point))
                      (form (read (current-buffer))))
                  (when (and (listp form)
                             (symbolp (car form))
                             (memq (car form) eai-code-reload--definition-types))
                    (let ((name (and (> (length form) 1)
                                     (if (eq (car form) 'cl-defstruct)
                                         (if (listp (cadr form))
                                             (cadr form)
                                           (cadr form))
                                       (cadr form)))))
                      (when (symbolp name)
                        (push (list name (car form)
                                    (buffer-substring-no-properties start (point)))
                              forms))))))
            ((end-of-file scan-error)
             nil))
          (nreverse forms))))))

(defun eai-code-reload--uncommitted-el-files ()
  "Return a list of Elisp files with uncommitted changes in the current project.
Uses `git diff --name-only' and filters for .el files."
  (let ((default-directory (or (when-let* ((proj (project-current)))
                                 (project-root proj))
                               default-directory))
        files)
    (with-temp-buffer
      (when (zerop (call-process "git" nil t nil "diff" "--name-only" "--diff-filter=AM"))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (string-trim (thing-at-point 'line t))))
            (when (string-suffix-p ".el" line)
              (push (expand-file-name line default-directory) files)))
          (forward-line 1))
        (nreverse files)))))

(defun eai-code-reload--eval-form-string (form-string file)
  "Evaluate FORM-STRING, setting `load-file-name' to FILE.
Returns the result or signals an error."
  (let ((load-file-name file))
    (eval (read form-string) t)))

(defun eai-code-reload--feature-for-file (file)
  "Return the feature symbol that FILE provides, or nil.
Reads forms from FILE until it finds a `(provide \='NAME)' form."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while (not (eobp))
            (let ((form (read (current-buffer))))
              (when (and (listp form)
                         (eq (car form) 'provide)
                         (> (length form) 1)
                         (symbolp (cadr form)))
                (cl-return (cadr form)))))
        ((end-of-file scan-error)
         nil)))))

(defun eai-code-reload--unbind (name type)
  "Remove the binding for NAME based on TYPE.
For functions, uses `fmakunbound'.  For variables, uses `makunbound'."
  (cond
   ((memq type '(defun defun* defsubst defmacro defalias
                       cl-defun cl-defmacro cl-defgeneric cl-defmethod
                       defgeneric defmethod))
    (fmakunbound name))
   ((memq type '(defvar defconst defcustom))
    (makunbound name))
   ((eq type 'defface)
    (face-spec-reset-face name))
   (t
    ;; Best effort: try both
    (ignore-errors (fmakunbound name))
    (ignore-errors (makunbound name)))))

;;; User commands

;;;###autoload
(defun eai-code-reload ()
  "Reload all eai-code and eai-tool-library source files.
Evaluates every .el file listed in `eai-code-reload-files'.
Files are looked up relative to the current project root.
Registers each touched name in `eai-code-reload--registry'.
Intended for interactive development without restarting Emacs.

Returns a summary message."
  (interactive)
  (let* ((project (or (project-current)
                      (error "Not in a project")))
         (root (project-root project))
         (evaluated 0)
         (errors 0)
         (new-names nil))
    (dolist (rel eai-code-reload-files)
      (let ((file (expand-file-name rel root)))
        (when (file-readable-p file)
          (message "Reloading %s..." file)
          (let ((forms (eai-code-reload--top-level-forms file)))
            (dolist (entry forms)
              (cl-destructuring-bind (name _type form-string) entry
                (condition-case err
                    (progn
                      (eai-code-reload--eval-form-string form-string file)
                      (setq evaluated (1+ evaluated))
                      (push name new-names)
                      (setf (alist-get name eai-code-reload--registry nil nil #'eq) file))
                  (error
                   (message "Error reloading %s from %s: %S" name file err)
                   (setq errors (1+ errors))))))))))
    (message "Reloaded %d definitions (%d errors) from %d files."
             evaluated errors (length eai-code-reload-files))
    (when new-names
      (message "Touched: %s" (mapconcat #'symbol-name new-names ", ")))))

;;;###autoload
(defun eai-code-reload-full ()
  "Fully reload all eai-code and eai-tool-library source files.

Unlike `eai-code-reload', this uses the standard Emacs `load'
mechanism rather than evaluating individual forms.  It:

1. Collects files from `eai-code-reload-files'
2. Unprovides any `eai-*' features they provide
3. Adds the project root to `load-path'
4. Loads each file with `load' (so `require', macros,
   `eval-when-compile' all work normally)
5. Reports errors per-file but continues with remaining files

This catches load-order and compile-time errors that the
incremental reload might mask, at the cost of being slightly
slower.  Use this when you want confidence that the code will
work after an Emacs restart."
  (interactive)
  (let* ((project (or (project-current)
                      (error "Not in a project")))
         (root (project-root project))
         (loaded 0)
         (errors 0)
         (old-load-path load-path)
         (features-to-unprovide nil))
    ;; Collect features to unprovide
    (dolist (rel eai-code-reload-files)
      (let* ((file (expand-file-name rel root))
             (feature (eai-code-reload--feature-for-file file)))
        (when feature
          (push feature features-to-unprovide))))
    ;; Remove from features list
    (dolist (feat features-to-unprovide)
      (setq features (remove feat features)))
    ;; Remove from load-history so require actually reloads
    (setq load-history
          (cl-remove-if (lambda (entry)
                          (let ((file-or-feature (car entry)))
                            (or (member file-or-feature features-to-unprovide)
                                (and (stringp file-or-feature)
                                     (cl-some (lambda (rel)
                                                (string-suffix-p rel file-or-feature))
                                              eai-code-reload-files)))))
                        load-history))
    ;; Add project root to load-path temporarily
    (add-to-list 'load-path root)
    ;; Sort files: eai-code-gptel-stub first, eai-code.el last
    (let* ((sorted-files
            (sort (copy-sequence eai-code-reload-files)
                  (lambda (a b)
                    (cond
                     ((string= a "eai-code-gptel-stub.el") t)
                     ((string= b "eai-code-gptel-stub.el") nil)
                     ((string= a "eai-code.el") nil)
                     ((string= b "eai-code.el") t)
                     (t (string< a b)))))))
      (dolist (rel sorted-files)
        (let ((file (expand-file-name rel root)))
          (when (file-readable-p file)
            (message "Loading %s..." file)
            (condition-case err
                (progn
                  (load file nil t t)
                  (setq loaded (1+ loaded)))
              (error
               (message "Error loading %s: %S" file err)
               (setq errors (1+ errors))))))))
    ;; Restore load-path
    (setq load-path old-load-path)
    (message "Full reload: %d files loaded, %d errors."
             loaded errors)))

;;;###autoload
(defun eai-code-reload-uncommitted ()
  "Evaluate all top-level definitions from uncommitted Elisp changes.
Registers each touched name so it can be reverted later.
Returns a summary message."
  (interactive)
  (let ((files (eai-code-reload--uncommitted-el-files))
        (evaluated 0)
        (errors 0)
        (new-names nil))
    (dolist (file files)
      (message "Reloading %s..." file)
      (let ((forms (eai-code-reload--top-level-forms file)))
        (dolist (entry forms)
          (cl-destructuring-bind (name _type form-string) entry
            (condition-case err
                (progn
                  (eai-code-reload--eval-form-string form-string file)
                  (setq evaluated (1+ evaluated))
                  (push name new-names)
                  (setf (alist-get name eai-code-reload--registry nil nil #'eq) file))
              (error
               (message "Error reloading %s from %s: %S" name file err)
               (setq errors (1+ errors)))))))
      (message "Reloaded %d definitions (%d errors) from %d files."
               evaluated errors (length files))
      (when new-names
        (message "Touched: %s" (mapconcat #'symbol-name new-names ", "))))))

;;;###autoload
(defun eai-code-reload-revert ()
  "Revert all names in the reload registry to their committed (HEAD) versions.
Names that did not exist in HEAD are unbound.  Clears the registry afterwards."
  (interactive)
  (let ((reverted 0)
        (unbound 0)
        (errors 0)
        (_not-found 0))
    (dolist (entry eai-code-reload--registry)
      (cl-destructuring-bind (name . file) entry
        (let* ((committed-forms (eai-code-reload--committed-forms file))
               (old (cl-find-if (lambda (e) (eq (car e) name)) committed-forms)))
          (if old
              (condition-case err
                  (progn
                    (eai-code-reload--eval-form-string (nth 2 old) file)
                    (setq reverted (1+ reverted)))
                (error
                 (message "Error reverting %s: %S" name err)
                 (setq errors (1+ errors))))
            ;; Not found in committed version: unbind
            (let* ((current-forms (eai-code-reload--top-level-forms file))
                   (current (cl-find-if (lambda (e) (eq (car e) name)) current-forms))
                   (type (and current (cadr current))))
              (eai-code-reload--unbind name type)
              (setq unbound (1+ unbound)))))))
    (setq eai-code-reload--registry nil)
    (message "Reverted %d definitions, unbound %d, %d errors."
             reverted unbound errors)))

;;;###autoload
(defun eai-code-reload-status ()
  "Show the current reload registry in a buffer."
  (interactive)
  (let ((buf (get-buffer-create "*eai-code reload status*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "# eai-code Reload Registry\n\n")
      (if (null eai-code-reload--registry)
          (insert "No names registered.\n")
        (insert (format "| Name | File | Committed? |\n"))
        (insert (format "|------|------|------------|\n"))
        (dolist (entry eai-code-reload--registry)
          (cl-destructuring-bind (name . file) entry
            (let* ((committed-forms (eai-code-reload--committed-forms file))
                   (in-committed (cl-find-if (lambda (e) (eq (car e) name))
                                             committed-forms)))
              (insert (format "| `%s` | %s | %s |\n"
                              name
                              (file-relative-name file)
                              (if in-committed "yes" "no (will unbind)"))))))))
    (pop-to-buffer buf)))

;;;###autoload
(defun eai-code-reload-clear ()
  "Clear the reload registry without reverting anything."
  (interactive)
  (setq eai-code-reload--registry nil)
  (message "Reload registry cleared."))

;;;###autoload
(defun eai-code-reload-file (file)
  "Reload definitions from a single FILE, registering touched names.
Interactively, prompts for a file."
  (interactive "fReload file: ")
  (let ((file (expand-file-name file))
        (evaluated 0)
        (errors 0))
    (message "Reloading %s..." file)
    (let ((forms (eai-code-reload--top-level-forms file)))
      (dolist (entry forms)
        (cl-destructuring-bind (name _type form-string) entry
          (condition-case err
              (progn
                (eai-code-reload--eval-form-string form-string file)
                (setq evaluated (1+ evaluated))
                (setf (alist-get name eai-code-reload--registry nil nil #'eq) file))
            (error
             (message "Error reloading %s from %s: %S" name file err)
             (setq errors (1+ errors)))))))
    (message "Reloaded %d definitions (%d errors) from %s."
             evaluated errors file)))

(provide 'eai-code-reload)
;;; eai-code-reload.el ends here
