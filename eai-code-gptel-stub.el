;;; eai-code-gptel-stub.el --- Stub declarations for gptel -*- lexical-binding: t; -*-
;;
;; This file exists only to silence byte-compiler warnings when
;; compiling eai-code modules against gptel.  It provides dummy
;; function definitions and variable declarations that are active
;; only when the real gptel symbols are not yet loaded.
;;
;; To use:
;;   (eval-when-compile (require 'eai-code-gptel-stub))
;;   (eai-code-gptel-stub--vars)
;;
;;; Code:

;;; Variables — use the macro in each file that references them.

(defmacro eai-code-gptel-stub--vars ()
  "Expand to `defvar' declarations for gptel variables.
Call this at the top level of any file that references gptel
variables to suppress byte-compiler warnings."
  '(progn
     (defvar gptel-backend)
     (defvar gptel-model)
     (defvar gptel-stream)
     (defvar gptel-mode)
     (defvar gptel-tools)
     (defvar gptel-use-tools)
     (defvar gptel-max-tokens)
     (defvar gptel-context)
     (defvar gptel-use-context)
     (defvar gptel-include-reasoning)
     (defvar gptel-org-branching-context)
     (defvar gptel-prompt-prefix-alist)
     (defvar gptel-response-prefix-alist)
     (defvar gptel-directives)
     (defvar gptel-prompt-transform-functions)
     (defvar gptel-post-response-functions)
     (defvar gptel--system-message)
     (defvar gptel--fsm-last)
     (defvar gptel--request-alist)
     (defvar gptel--num-messages-to-send)
     (defvar gptel--preset)
     (defvar gptel--known-backends)
     (defvar gptel--tool-preview-alist)
     (defvar gptel-pre-send-functions)
     (defvar eai-code--current-profile)))

;;; Also declare them in the stub file itself
(eai-code-gptel-stub--vars)

;;; Functions (only defined if not already present)

;; Core request and send
(unless (fboundp 'gptel-send)
  (defun gptel-send (&optional _arg)))

(unless (fboundp 'gptel-mode)
  (defun gptel-mode (&optional _arg)))

(unless (fboundp 'gptel-abort)
  (defun gptel-abort (_buf)))

(unless (fboundp 'gptel-request)
  (defun gptel-request (&optional _prompt &rest _keys)))

;; Backend access
(unless (fboundp 'gptel-get-backend)
  (defun gptel-get-backend (_name)))

(unless (fboundp 'gptel-backend-name)
  (defun gptel-backend-name (_backend)))

;; Utility
(unless (fboundp 'gptel--to-string)
  (defun gptel--to-string (_object)))

(unless (fboundp 'gptel--insert-response)
  (defun gptel--insert-response (_response _info)))

(unless (fboundp 'gptel--handle-error)
  (defun gptel--handle-error (_fsm)))

(unless (fboundp 'gptel--apply-preset)
  (defun gptel--apply-preset (_preset &optional _set-fn)))

(unless (fboundp 'gptel--preset-syms)
  (defun gptel--preset-syms (_preset)))

(unless (fboundp 'gptel--update-status)
  (defun gptel--update-status (_status _face)))

(unless (fboundp 'gptel--parse-directive)
  (defun gptel--parse-directive (_directive &optional _no-insert)))

(unless (fboundp 'gptel--create-prompt-buffer)
  (defun gptel--create-prompt-buffer (&optional _prompt-end)))

(unless (fboundp 'gptel--parse-buffer)
  (defun gptel--parse-buffer (&rest _args)))

(unless (fboundp 'gptel--transform-add-context)
  (defun gptel--transform-add-context (_prompt)))

;; Org mode
(unless (fboundp 'gptel--convert-markdown->org)
  (defun gptel--convert-markdown->org (_str)))

(unless (fboundp 'gptel--stream-convert-markdown->org)
  (defun gptel--stream-convert-markdown->org (_start-marker)))

;; FSM
(unless (fboundp 'gptel-fsm-info)
  (defun gptel-fsm-info (_fsm)))

(unless (fboundp 'gptel-make-fsm)
  (defun gptel-make-fsm (&rest _props)))

;; Agent
(unless (fboundp 'gptel-agent--task)
  (defun gptel-agent--task (_main-cb _agent-type _description _prompt)))

;; Tools
(unless (fboundp 'gptel-get-tool)
  (defun gptel-get-tool (_name)))

(unless (fboundp 'gptel-tool-name)
  (defun gptel-tool-name (_tool)))

(unless (fboundp 'gptel-tool-function)
  (defun gptel-tool-function (_tool)))

(unless (fboundp 'gptel--send-tool-result)
  (defun gptel--send-tool-result (_id _result)))

(unless (fboundp 'gptel-make-tool)
  (defun gptel-make-tool (&rest _args)))

(unless (fboundp 'gptel-tool-args)
  (defun gptel-tool-args (_tool)))

(unless (fboundp 'gptel--format-tool-call)
  (defun gptel--format-tool-call (_name _args)))

(unless (fboundp 'gptel--display-tool-calls)
  (defun gptel--display-tool-calls (_calls _info)))

;;; Macros (dummy definition)

(unless (fboundp 'gptel-with-preset)
  (defmacro gptel-with-preset (name &rest body)
    `(progn ,name ,@body)))

(provide 'eai-code-gptel-stub)
;;; eai-code-gptel-stub.el ends here
