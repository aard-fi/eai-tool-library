;;; eai-code.el --- Quick tool to check your setup -*- lexical-binding: t; -*-
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
;; The doctor does a health check, and tries to give you useful tips on how
;; go get better
;;
;;; Code:

(require 'project)
(require 'subr-x)
(eval-when-compile
  (require 'eai-code-gptel-stub)
  (eai-code-gptel-stub--vars))

(defvar eai-doctor--ok      (propertize "✔" 'face 'success))
(defvar eai-doctor--fail    (propertize "✘" 'face 'error))
(defvar eai-doctor--warning (propertize "!" 'face 'warning))
(defvar eai-doctor--info    (propertize "i" 'face 'shadow))

(defun eai-doctor--treesit-find-grammar-path (lang)
  "Return the absolute path to the grammar file for LANG, or nil."
  (let* ((lib-name (format "libtree-sitter-%s%s"
                           lang (cl-case system-type
                                  (windows-nt ".dll")
                                  (darwin ".dylib")
                                  (t ".so"))))
         ;; Combine Emacs-specific paths and standard system paths
         (search-paths (append treesit-extra-load-path
                               (list (expand-file-name "tree-sitter" user-emacs-directory)
                                     "/usr/local/lib"
                                     "/usr/lib"))))
    (cl-loop for path in search-paths
             for full-path = (expand-file-name lib-name path)
             if (file-exists-p full-path)
             return full-path)))

(defun eai-doctor ()
  "Generate a diagnostic report for eai."
  (interactive)
  (let ((buf (get-buffer-create "*eai-doctor*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Emacs Assisted Intelligence, health check\n" 'face '(:inherit warning :weight bold)))
        (insert (propertize (make-string 42 ?─) 'face 'shadow) "\n\n")

        ;; --- ADD YOUR DIAGNOSTIC MODULES HERE ---
        (eai-doctor--check-gptel)
        (eai-doctor--check-llm)
        (eai-doctor--check-environment)
        (eai-doctor--check-treesitter)
        (eai-doctor--check-imenu)

        (eai-doctor--insert-trailer)

        ;; Buffer Setup
        (goto-char (point-min))
        (unless (eq major-mode 'special-mode)
          (special-mode)
          (local-set-key (kbd "q")
                         (lambda ()
                           (interactive)
                           (let ((window (selected-window)))
                             ;; 'kill' means kill the buffer, 'window' is the window to quit.
                             ;; This avoids the frame-killing logic often found in 'quit-window'.
                             (quit-restore-window window 'kill))))
          (setq-local buffer-read-only t)
          (setq-local revert-buffer-functionq
                      (lambda (&rest _args) (eai-doctor)))
          (set-buffer-modified-p nil)))
      (pop-to-buffer buf))))

(defun eai-doctor--check-llm ()
  "Insert llm library and provider diagnostics."
  (insert (propertize "● llm library status\n" 'face 'bold))
  (if (locate-library "llm")
      (insert (format "  [%s] llm library is available.\n" eai-doctor--ok))
    (insert (format "  [%s] llm library is NOT in load path.
      You can get it from https://elpa.gnu.org/packages/llm.html\n"
                    eai-doctor--fail)))

  (if (not (featurep 'llm))
      (insert (format "  [%s] llm library is NOT loaded.\n" eai-doctor--fail))
    (insert (format "  [%s] llm library is loaded.\n" eai-doctor--ok))

    (let ((providers '())
          (all-providers '(llm-openai llm-ollama llm-llama-cpp llm-anthropic llm-gemini)))
      (dolist (p all-providers)
        (when (featurep p)
          (push p providers)))

      (insert "  Loaded Providers: ")
      (if providers
          (dolist (p providers)
            (insert (propertize (format "%s " p) 'face 'success)))
        (insert (propertize "None (Did you require a provider like 'llm-openai?)" 'face 'shadow)))
      (insert "\n"))

    ;; Replace 'eai-code-llm-provider' with your actual variable name
    (if (and (boundp 'eai-code-llm-provider) eai-code-llm-provider)
        (insert (format "  [%s] Active Project Provider: %s\n"
                        eai-doctor--ok
                        (type-of eai-code-llm-provider)))
      (insert (format "  [%s] No project-specific llm-provider variable is currently set.\n"
                      eai-doctor--warning))))
  (insert "\n"))

(defun eai-doctor--check-gptel ()
  "Insert gptel configuration diagnostics."
  (insert (propertize "● gptel library status\n" 'face 'bold))
  (if (locate-library "gptel")
      (insert (format "  [%s] gptel library is available.\n" eai-doctor--ok))
    (insert (format "  [%s] gptel library is NOT in load path.
      You can get it from https://github.com/karthink/gptel\n"
                    eai-doctor--fail)))

  (if (not (featurep 'gptel))
      (insert (format "  [%s] gptel library is NOT loaded.
      Configure it according to https://gptel.org/\n"
                      eai-doctor--fail))

    (if (not gptel-backend)
        (insert (format "  [%s] No gptel-backend configured.\n" eai-doctor--fail))
      (insert (format "  [%s] Current Backend: %s\n" eai-doctor--ok
                      (if (stringp gptel-backend) gptel-backend (cl-struct-slot-value 'gptel-backend 'name gptel-backend)))))

    (if (not gptel-model)
        (insert (format "  [%s] No gptel-model selected.\n" eai-doctor--fail))
      (insert (format "  [%s] Current Model:   %s\n" eai-doctor--ok gptel-model)))

    (insert "  Registered Providers: ")
    (if (and (boundp 'gptel--known-backends) gptel--known-backends)
        (dolist (item gptel--known-backends)
          (let ((name (car item)))
            (insert (propertize (format "%s " name) 'face 'success))))
      (insert (propertize "None found" 'face 'shadow)))
    (insert "\n"))
  (insert "\n"))

(defun eai-doctor--check-imenu (&optional all-modes)
  "Identify Imenu support. Filters for programming/TS modes by default.
If ALL-MODES is non-nil, shows every mode found."
  (insert (propertize (format "● imenu support (%s)\n"
                              (if all-modes "system-wide" "programming/ts")) 'face 'bold))
  (insert "\nThis is a list of imenu enabled programming modes. A modern Emacs should have that enabled in most modes - if this is empty there might be a bigger problem.\n\n")
  (let (supported-modes)
    (mapatoms
     (lambda (s)
       (let ((name (symbol-name s)))
         (when (and (fboundp s)
                    (string-match-p "-mode$" name)
                    (not (string-match-p "--" name)))
           (let* ((is-ts (string-match-p "-ts-mode$" name))
                  (mode-base (string-remove-suffix "-mode" name))
                  ;; Heritage check: include if it's a TS mode or derived from prog/text
                  (parent (get s 'derived-mode-parent))
                  (is-relevant (or all-modes
                                   is-ts
                                   (memq s '(emacs-lisp-mode lisp-mode prog-mode text-mode))
                                   (memq parent '(prog-mode text-mode))))
                  ;; Detection logic
                  (elisp-check (and (eq s 'emacs-lisp-mode)
                                    (boundp 'lisp-imenu-generic-expression)))
                  (has-prop (or (get s 'imenu-generic-expression)
                                (get s 'imenu-create-index-function)))
                  (type (cond
                         (is-ts "Tree-sitter (Native)")
                         (elisp-check "Regex (Lisp Standard)")
                         (has-prop "Regex (Property)")
                         ((boundp (intern-soft (concat mode-base "-imenu-generic-expression"))) "Regex (Variable)")
                         (nil nil))))
             (when (and type is-relevant)
               (push (list s type) supported-modes)))))))

    (setq supported-modes (sort supported-modes
                                (lambda (a b) (string< (symbol-name (car a))
                                                       (symbol-name (car b))))))

    (if (not supported-modes)
        (insert "  No matching modes detected.\n")
      (dolist (entry supported-modes)
        (insert (format "  %-30s %s\n"
                        (propertize (symbol-name (car entry)) 'face 'success)
                        (propertize (concat "→ " (cadr entry)) 'face 'shadow)))))
    (insert "\n")))

(defun eai-doctor--check-treesitter ()
  "Insert tree-sitter specific diagnostics."
  (insert (propertize "● tree-sitter status\n" 'face 'bold))
  (let ((ts-builtin (treesit-available-p))
        (ts-available (and (featurep 'treesit) (treesit-available-p))))
    (if ts-builtin
        (insert (format "  [%s] tree-sitter is built in.\n" eai-doctor--ok))
      (insert (format "  [%s] tree-sitter is NOT available. (Check Emacs version or libtree-sitter)\n" eai-doctor--fail)))

    (if ts-available
        (insert (format "  [%s] tree-sitter library is loaded.\n" eai-doctor--ok))
      (insert (format "  [%s] tree-sitter is NOT loaded.\n" eai-doctor--fail)))

    (when ts-available
      (insert (format "  [%s] major-mode-remap-alist %s\n"
                      (if major-mode-remap-alist eai-doctor--ok eai-doctor--warning)
                      (if major-mode-remap-alist "is set"
                        "should be set for automatic ts mode switching")))
      (insert "\n  tree-sitter enabled modes:\n")
      (let ((active-ts-modes '())
            (potential-ts-modes '()))
        (mapatoms (lambda (s)
                    (let ((name (symbol-name s)))
                      (when (string-match-p "-ts-mode$" name)
                        (if (fboundp s)
                            (push s active-ts-modes)
                          (push s potential-ts-modes))))))

        (setq active-ts-modes (sort active-ts-modes (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
        (dolist (mode active-ts-modes)
          (let* ((lang (intern (string-remove-suffix "-ts-mode" (symbol-name mode))))
                 (grammar-path (or (eai-doctor--treesit-find-grammar-path lang) "grammar not found")))
            (insert (format "  [%s] %s %s\n"
                            (if (treesit-language-available-p lang) eai-doctor--ok eai-doctor--fail)
                            mode grammar-path))))))
    (insert "\n")))

(defun eai-doctor--check-environment ()
  "Check system environment for eai-code dependencies."
  (insert (propertize "● System Dependencies\n" 'face 'bold))
  ;; Check for git (essential for project.el usually)
  (let ((git-path (executable-find "git")))
    (if git-path
        (insert (format "  [%s] Git: Found at %s\n" eai-doctor--ok git-path))
      (insert (format
               "  [%s] Git: NOT FOUND. project.el might struggle to find roots.\n"
               eai-doctor--fail))))

  ;; Check for custom root markers (Emacs 29+)
  (if (boundp 'project-vc-extra-root-markers)
      (insert (format "  [%s] project-vc-extra-root-markers: %S\n" eai-doctor--info project-vc-extra-root-markers))
    (insert (format "  [%s] project-vc-extra-root-markers: Not available (Requires Emacs 29+)\n" eai-doctor--info)))

  ;; Check for VC ignores
  (if (boundp 'project-vc-ignores)
      (insert (format "  [%s] project-vc-ignores: %S\n" eai-doctor--info project-vc-ignores))
    (insert (format "  [%s] project-vc-ignores: Not defined\n" eai-doctor--info)))
  (insert "\n"))

(defun eai-doctor--insert-trailer ()
  "Insert a stylized footer for the report."
  (insert "\n" (propertize (make-string 40 ?─) 'face 'shadow) "\n")
  (insert (propertize "End of report. \n" 'face 'shadow))
  (insert (propertize "Press 'q' to close this buffer, 'g' to refresh.\n" 'face 'italic))
  (insert "\n" (propertize  "Please remember to submit your health insurance information." 'face 'shadow))
  (insert "\n" (propertize  "US residents may alternatively submit their gofundme page." 'face 'shadow))
  (insert "\n" (propertize (format "Generated at: %s" (current-time-string)) 'face 'shadow)))

(provide 'eai-doctor)

;;; eai-doctor.el ends here
