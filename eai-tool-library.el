;;; eai-tool-library.el --- A collection of tools for gptel -*- lexical-binding: t; -*-
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
;; This implements various tools for LLMS, and implements methods for easy
;; loading and unloading
;;
;;; Code:

(require 'cl-lib)
(require 'project)

(defgroup eai-tool-library nil
  "eai-tool-library settings"
  :group 'eai-tool-library)

(defcustom eai-tool-library-use t
  "Use tools which are safe to use"
  :group 'eai-tool-library
  :type 'sexp)

(defcustom eai-tool-library-use-maybe-safe nil
  "Use tools which are most likely safe to use"
  :group 'eai-tool-library
  :type 'sexp)

(defcustom eai-tool-library-use-unsafe nil
  "Use tools which are unsafe"
  :group 'eai-tool-library
  :type 'sexp)

(defcustom eai-tool-library-max-result-size 40
  "The maximum length of a result in characters.

Depending no the function exceeding that should either throw an error, or filter
the result"
  :group 'eai-tool-library
  :type 'integer)

(defcustom eai-tool-library-gptel-tools-var 'gptel-tools
  "Symbol of the variable holding the list of GPTel tools."
  :group 'eai-tool-library
  :type 'symbol)

(defcustom eai-tool-library-llm-tools-var nil
  "Symbol of the variable holding the list of LLM tools."
  :group 'eai-tool-library
  :type 'symbol)

(defcustom eai-tool-library-debug t
  "Log messages to `eai-tool-library-debug-buffer' when non-nil`"
  :group 'eai-tool-library
  :type 'symbol)

(defcustom eai-tool-library-debug-buffer "*eai-tool-debug*"
  "Buffer for debug output, if enabled via `eai-tool-library-debug'"
  :group 'eai-tool-library
  :type 'string)

(defconst eai-tool-library-dir (file-name-directory (or load-file-name
                                                        buffer-file-name)))

(defun eai-tool-library-append-tools (var tools)
  "Append TOOLS to the list stored in variable VAR non-destructively."
  (set var (append (symbol-value var) tools)))

(defun eai-tool-library--debug-log (log)
  "Print LOG to `eai-tool-library-debug-buffer' if debug logging is enabled."
  (let ((_buffer (get-buffer-create eai-tool-library-debug-buffer)))
    (with-current-buffer eai-tool-library-debug-buffer
      (insert (format "%s\n" log)))))

(defun eai-tool-library--limit-result (result)
  (if (>= (length (format "%s" result)) eai-tool-library-max-result-size)
      (format "Results over %s character. Stop. Analyze. Find a different solution, or use a more specific query." eai-tool-library-max-result-size)
    result))

(defun eai-tool-library-make-tools (&rest args)
  "Create tools for any known LLMs"
  (let ((tool-list '()))
    (when (fboundp 'gptel-make-tool)
      (cl-pushnew (apply #'gptel-make-tool args) tool-list))
    (when (fboundp 'llm-make-tool)
      (cl-pushnew (apply #'llm-make-tool args) tool-list))
    (when (fboundp 'claude-code-ide-make-tool)
      ;; TODO, this needs some work
      (condition-case err
          (let ((tool (apply #'claude-code-ide-make-tool args)))
            (message "claude-code-ide-make-tool returned: %S" tool)
            (when (memq (type-of tool) '(gptel-tool llm-tool))
              (cl-pushnew tool tool-list)))
        (error (message "Skipping broken claude tool: %s" err))))
    tool-list))

(defun eai-tool-library-make-tools-and-register (list &rest args)
  "Create tools for any known LLMS and add them to LIST"
  (eai-tool-library-append-tools list
                                 (apply #'eai-tool-library-make-tools args)))

(defun eai-tool-library-tool--accessor (tool slot)
  "Return SLOT value from TOOL if it is a known type and accessor exists, else nil.

This helps us in providing a generic wrapper for the tool interfaces of the
various LLM libraries"
  (cond
   ((and (cl-typep tool 'gptel-tool)
         (fboundp (intern (format "gptel-tool-%s" slot))))
    (funcall (intern (format "gptel-tool-%s" slot)) tool))
   ((and (cl-typep tool 'llm-tool)
         (fboundp (intern (format "llm-tool-%s" slot))))
    (funcall (intern (format "llm-tool-%s" slot)) tool))
   (t nil)))

;; the following there are probably the ones we care about most for searching
;; modified tools for removal
(defun eai-tool-library-tool-function (tool)
  "Convenience binding to get the `function' slot out of a tool"
  (eai-tool-library-tool--accessor tool "function"))

(defun eai-tool-library-tool-name (tool)
  "Convenience binding to get the `name' slot out of a tool"
  (eai-tool-library-tool--accessor tool "name"))

(defun eai-tool-library-tool-category (tool)
  "Convenience binding to get the `category' slot out of a tool"
  (eai-tool-library-tool--accessor tool "category"))

(cl-defun eai-tool-library--remove-tool-by-keys (tools-list &key function name category)
  "Remove tools from TOOLS-LIST (a symbol) matching keys FUNCTION, NAME, CATEGORY.
Ignores tools for which required accessors are not available."
  (setf (symbol-value tools-list)
        (seq-remove
         (lambda (tool)
           (let ((tool-fn (eai-tool-library-tool-function tool))
                 (tool-name (eai-tool-library-tool-name tool))
                 (tool-cat (eai-tool-library-tool-category tool)))
             (and tool-fn tool-name  ;; skip if can't get necessary info
                  (and (or (not function) (eq tool-fn function))
                       (or (not name) (string= tool-name name))
                       (or (not category) (string= tool-cat category))))))
         (symbol-value tools-list))))

(defun eai-tool-library--remove-tool-by-keys-everywhere (&rest args)
  "Remove the tool from all known tool lists"
  (when (and (boundp eai-tool-library-gptel-tools-var)
             eai-tool-library-gptel-tools-var)
    (apply #'eai-tool-library--remove-tool-by-keys
           (cons (symbol-value 'eai-tool-library-gptel-tools-var) args)))
  (when (and (boundp eai-tool-library-llm-tools-var)
             eai-tool-library-llm-tools-var)
    (apply #'eai-tool-library--remove-tool-by-keys
           (cons (symbol-value 'eai-tool-library-llm-tools-var) args))))

(defun eai-tool-library--register-tool (tool)
  "Register the tool to the type appropriate list, or skip."
  (let* ((tool-type (pcase (type-of tool)
                      ('gptel-tool "gptel")
                      ('llm-tool "llm")
                      (_ (error "Unknown tool type encountered: %s" (type-of tool)))))
         (container-var-sym (intern (format "eai-tool-library-%s-tools-var" tool-type))))
    (when (and (boundp container-var-sym)
               (symbol-value container-var-sym))
      (let ((target-var-sym (symbol-value container-var-sym)))
        (unless (listp (symbol-value target-var-sym))
          (set target-var-sym '()))
        (set target-var-sym (cons tool (symbol-value target-var-sym)))))))

(defun eai-tool-library-load-module (module-name)
  "Load `eai-tool-library-MODULE-NAME' and add its tools to `gptel-tools'.

Note that this unloads all tools from a module before loading - so for a reload
it is sufficient to just call this function, explicit unloading is not required."
  (let* ((module-sym (intern (format "eai-tool-library-%s" module-name))))
    (condition-case err
        (progn
          (eai-tool-library-unload-module module-name)
          (require module-sym)
          (dolist (n '("" "-unsafe" "-maybe-safe"))
            (let* ((cond-var-sym (intern (format "eai-tool-library-use%s" n)))
                   (tool-var-sym (intern (format "eai-tool-library-%s-tools%s" module-name n)))
                   (module-category-sym (intern (format "eai-tool-library-%s-category-name" module-name)))
                   (category (if (boundp module-category-sym)
                                 (symbol-value module-category-sym)
                               module-name)))
              (when (and (boundp cond-var-sym) (symbol-value cond-var-sym))
                (when (boundp tool-var-sym)
                  (dolist (tool (symbol-value tool-var-sym))
                    ;; refuse loading if categories are incorrectly defined
                    (when (eq (type-of tool) 'gptel-tool)
                      (let ((tool-category (eai-tool-library-tool-category tool)))
                        (eai-tool-library--debug-log
                         (format "Processing tool load for %s..."
                                 (eai-tool-library-tool-name tool)))
                        (unless
                            (string=
                             (if (symbolp tool-category) (symbol-name tool-category) tool-category)
                             (if (symbolp category) (symbol-name category) category))
                          (error
                           (message
                            (format "Category %s does not match tool category %s"
                                    category tool-category))))))
                    (eai-tool-library--register-tool tool)))))))
      (error (message "Error loading module %s: %s" module-name err)))))

(defun eai-tool-library-unload-module (module-name)
  "Remove tools from `gptel-tools' that belong to module MODULE-NAME.

This expects that all tools from the module have the same category, and the
category either matches the module name, or a `category-name' variable is set in
the module namespace."
  (let* ((module-category-sym (intern (format "eai-tool-library-%s-category-name" module-name)))
         (category (if (boundp module-category-sym)
                       (symbol-value module-category-sym)
                     module-name)))
    ;; TODO: for llm we should loop through existing variables here to remove tools
    (let* ((feature-name (format "eai-tool-library-%s" module-name))
           (feature-sym (intern feature-name)))
      (when (featurep feature-sym)
        (unload-feature feature-sym t)))
    ;; nuke all existing tools, by category - we don't know
    ;; if tool definitions have changed, so might no longer match
    ;; llm tools don't have categories, so we'll still have to try
    ;; to remove them by name individually later on
    (eai-tool-library--remove-tool-by-keys-everywhere :category category)))

;; generic helper functions

(defun eai-tool-library--get-buffer (buffer-or-path &optional create-p)
  "Get a buffer for BUFFER-OR-PATH, project-aware.

If BUFFER-OR-PATH is an existing buffer, returns it.
If BUFFER-OR-PATH is a string path, searches for a matching buffer by:
1. Exact file path match
2. Suffix match (BUFFER-OR-PATH is a trailing segment of the buffer's path)
3. Basename match (filename component only)
If no buffer exists, opens or creates the file resolved against the project
root. A string naming no file but a live non-file buffer, such as *scratch*,
returns that buffer.

If CREATE-P is non-nil and the file doesn't exist, creates it.

Returns the buffer object."
  (cond
   ((bufferp buffer-or-path) buffer-or-path)
   ((stringp buffer-or-path)
    (let* ((project-root (when-let* ((pr (project-current)))
                           (project-root pr)))
           (resolved-path (if (file-name-absolute-p buffer-or-path)
                              (expand-file-name buffer-or-path)
                            (if project-root
                                (expand-file-name buffer-or-path project-root)
                              (expand-file-name buffer-or-path))))
           ;; 1. Exact match
           (buffer (get-file-buffer resolved-path))
           ;; 2. Suffix match among live buffers (handles project-relative input)
           (buffer (or buffer
                       (catch 'found
                         (dolist (buf (buffer-list))
                           (when-let* ((bfn (buffer-file-name buf)))
                             (when (or (string-suffix-p buffer-or-path bfn)
                                       (string-suffix-p (concat "/" buffer-or-path) bfn))
                               (throw 'found buf)))))))
           ;; 3. Basename match
           (buffer (or buffer
                       (let ((basename (file-name-nondirectory buffer-or-path)))
                         (catch 'found
                           (dolist (buf (buffer-list))
                             (when-let* ((bfn (buffer-file-name buf)))
                               (when (string= basename (file-name-nondirectory bfn))
                                 (throw 'found buf)))))))))
      (or buffer
          (and (file-exists-p resolved-path)
               (find-file-noselect resolved-path))
          ;; not a file: a live non-file buffer such as *scratch*, by name
          (get-buffer buffer-or-path)
          (if create-p
              (let ((buf (generate-new-buffer (file-name-nondirectory resolved-path))))
                (with-current-buffer buf
                  (set-visited-file-name resolved-path))
                buf)
            (error "File %s does not exist" resolved-path)))))))

;; write permissions

(defcustom eai-tool-library-write-policy nil
  "Where tools may write, as an alist of (DIRECTORY . ACTION).
ACTION is `allow' (write without asking), `ask' (show the change to
the user for review) or `deny'.  The entry with the longest DIRECTORY
containing the target file wins.  Files matching no entry use
`eai-tool-library-write-in-project' inside the current project and
`eai-tool-library-write-outside-project' elsewhere.

Keep this in your own configuration: a project must not be able to
grant itself write access."
  :group 'eai-tool-library
  :type '(alist :key-type directory
                :value-type (choice (const allow) (const ask) (const deny))))

(defcustom eai-tool-library-write-in-project 'ask
  "Write action for files in the current project without a policy entry."
  :group 'eai-tool-library
  :type '(choice (const allow) (const ask) (const deny)))

(defcustom eai-tool-library-write-outside-project 'deny
  "Write action for files outside the current project without a policy entry."
  :group 'eai-tool-library
  :type '(choice (const allow) (const ask) (const deny)))

(defun eai-tool-library--path-in-directory-p (file dir)
  "Return non-nil if true name FILE is inside directory DIR."
  (string-prefix-p (file-name-as-directory (file-truename dir)) file
                   (file-name-case-insensitive-p file)))

(defun eai-tool-library-write-action (file)
  "Return the write action for FILE: `allow', `ask' or `deny'.
FILE nil stands for a buffer without a file, which is always `ask'.
The current project is the one of the current buffer; tools run in
the buffer their request came from.  See `eai-tool-library-write-policy'."
  (if (not file)
      'ask
    (let ((file (file-truename file))
          best)
      (dolist (entry eai-tool-library-write-policy)
        (when (and (eai-tool-library--path-in-directory-p file (car entry))
                   (or (not best)
                       (> (length (file-truename (car entry)))
                          (length (file-truename (car best))))))
          (setq best entry)))
      (cond
       (best (cdr best))
       ((when-let* ((project (project-current)))
          (eai-tool-library--path-in-directory-p file (project-root project)))
        eai-tool-library-write-in-project)
       (t eai-tool-library-write-outside-project)))))

(defun eai-tool-library-allow-project-writes (&optional save)
  "Let tools write in the current project without asking.
With prefix argument SAVE, also save the setting with Customize."
  (interactive "P")
  (let ((root (expand-file-name
               (or (when-let* ((project (project-current)))
                     (project-root project))
                   (user-error "Not in a project")))))
    (setf (alist-get root eai-tool-library-write-policy nil nil #'equal) 'allow)
    (when save
      (customize-save-variable 'eai-tool-library-write-policy
                               eai-tool-library-write-policy))
    (message "Tools may write in %s without asking%s"
             root (if save " (saved)" ""))))

(defun eai-tool-library-write-confirm-p (file)
  "Return non-nil if writing FILE needs user confirmation."
  (eq (eai-tool-library-write-action file) 'ask))

(defun eai-tool-library-write-deny-check (file)
  "Signal an error if writing FILE is denied by policy.
Does nothing if the action is `allow' or `ask'."
  (when (eq (eai-tool-library-write-action file) 'deny)
    (error "Write denied by policy: %s" file)))

(provide 'eai-tool-library)

;;; eai-tool-library.el ends here
