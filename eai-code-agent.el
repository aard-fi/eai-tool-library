;;; eai-code-agent.el --- Agent profiles and dispatch for eai-code -*- lexical-binding: t; -*-
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
;; Agent profiles with tiered backend/model selection.
;;
;; Each profile specifies a list of backends to try in order (cheap first).
;; When a backend is unavailable or fails, the next one is tried automatically.
;;
;; Key functions:
;; - `eai-code-agent--dispatch'      — try backends in order, return first success
;; - `eai-code--agent-task'          — wrap gptel-agent--task with profile selection
;; - `eai-code-delegate'             — interactive command to delegate a task
;; - `eai-code-agent--backend-stats' — track success/failure per backend
;;
;;; Code:

(require 'cl-lib)
(eval-when-compile
  (require 'eai-code-gptel-stub)
  (eai-code-gptel-stub--vars))
(require 'eai-code-gptel-stub)

(defvar eai-code--sub-agents)
(declare-function eai-code--update-header-line "eai-code")
(declare-function eai-code-agent--refresh "eai-code")

(defvar eai-code--current-profile nil
  "Current agent profile for the active request.
Set buffer-locally before dispatching a sub-agent task.")

(make-variable-buffer-local 'eai-code--current-profile)

(defgroup eai-code-agent nil
  "Agent profiles and dispatch for eai-code."
  :group 'eai-code)

(defcustom eai-code-agent-profiles
  '((default
     :description "Default profile for general tasks"
     :backends nil))
  "Agent profiles with tiered backend/model fallback lists.

Each element is (NAME . PLIST) where PLIST contains:
  :description — human-readable description
  :backends    — list of (BACKEND-NAME . MODEL-SYMBOL) pairs, tried in order
  :max-tokens  — optional max tokens for this profile

Example:
  ((summarizer
    :description \"Summarize text or compress context\"
    :backends ((\"ollama\" . llama3.2:3b)
               (\"anthropic\" . claude-sonnet-4-6))
    :max-tokens 2048)
   (researcher
    :description \"Explore code or web\"
    :backends ((\"ollama\" . qwen2.5-coder:14b)
               (\"anthropic\" . claude-sonnet-4-6)))
   (executor
    :description \"Execute multi-step code changes\"
    :backends ((\"anthropic\" . claude-opus-4-7)
               (\"anthropic\" . claude-sonnet-4-6)))
   (introspector
    :description \"Examine Emacs state\"
    :backends ((\"ollama\" . qwen2.5-coder:14b))))"
  :type '(alist :key-type symbol
                :value-type (plist :options
                                   ((:description string)
                                    (:backends (repeat (cons string symbol)))
                                    (:max-tokens integer))))
  :group 'eai-code-agent)

(defcustom eai-code-agent-timeout 30
  "Timeout in seconds for each backend attempt.
If a backend does not respond within this time, the next backend is tried."
  :type 'integer
  :group 'eai-code-agent)

;;; Stats tracking

(defvar eai-code-agent--stats nil
  "Alist of (BACKEND-NAME . (SUCCESS . FAILURE)) tracking per-backend stats.
Keys are backend names as strings.  Values are cons cells where
car is the number of successful requests and cdr is failures.

These stats are used to learn optimal first choices over time.
They are NOT persisted across Emacs sessions by default.")

(defun eai-code-agent--stats-record (backend-name success)
  "Record a SUCCESS or FAILURE for BACKEND-NAME."
  (let ((entry (assoc backend-name eai-code-agent--stats)))
    (if entry
        (if success
            (setcdr entry (cons (1+ (caadr entry)) (cdadr entry)))
          (setcdr entry (cons (caadr entry) (1+ (cdadr entry)))))
      (push (cons backend-name (cons (if success 1 0) (if success 0 1)))
            eai-code-agent--stats))))

(defun eai-code-agent--stats-get (backend-name)
  "Return (SUCCESS . FAILURE) stats for BACKEND-NAME, or (0 . 0)."
  (or (cdr (assoc backend-name eai-code-agent--stats)) '(0 . 0)))

;;; Profile accessors

(defun eai-code-agent--profile-get (profile key)
  "Get KEY from PROFILE's property list."
  (plist-get (cdr (assoc profile eai-code-agent-profiles)) key))

(defun eai-code-agent--profile-names ()
  "Return a list of all profile name symbols."
  (mapcar #'car eai-code-agent-profiles))

;;; Backend availability

(defun eai-code-agent--backend-available-p (backend-name)
  "Check if BACKEND-NAME is registered with gptel.
Returns t if available, nil otherwise.
Also records a failure stat if the backend is not found."
  (condition-case nil
      (and (fboundp 'gptel-get-backend)
           (gptel-get-backend backend-name)
           t)
    (error
     (eai-code-agent--stats-record backend-name nil)
     nil)))

;;; Dispatch with tiered fallback

(defun eai-code-agent--select-backend (profile)
  "Return the first available (BACKEND-NAME . MODEL) from PROFILE.

Iterates through the profile's `:backends' list and returns the
first backend that is registered with gptel.  Returns nil if none
are available."
  (let ((backends (eai-code-agent--profile-get profile :backends)))
    (cl-some (lambda (pair)
               (when (eai-code-agent--backend-available-p (car pair))
                 pair))
             backends)))

(defun eai-code-agent--dispatch (profile prompt callback)
  "Dispatch PROMPT through PROFILE's backends, trying each in order.

Calls CALLBACK with the result string on success, or an error
message string if all backends fail.

This is an async function: it returns immediately and the callback
is called when the request completes (or all backends have been
tried)."
  (let ((backends (copy-sequence (eai-code-agent--profile-get profile :backends)))
        (attempted nil))
    (cl-labels
        ((try-next
           ()
           (if (null backends)
               (funcall callback
                        (format "All backends failed for profile '%s'. Attempted: %s"
                                profile
                                (or (mapconcat (lambda (b)
                                                 (format "%s/%s" (car b) (cdr b)))
                                               (nreverse attempted)
                                               ", ")
                                    "none")))
             (let* ((next (pop backends))
                    (backend-name (car next))
                    (model (cdr next)))
               (push next attempted)
               (if (eai-code-agent--backend-available-p backend-name)
                   (let ((preset `(:backend ,backend-name :model ,model)))
                     (condition-case err
                         (gptel-with-preset preset
                           (gptel-request prompt
                             :callback
                             (lambda (resp _info)
                               (cond
                                ((null resp)
                                 (eai-code-agent--stats-record backend-name nil)
                                 (try-next))
                                ((stringp resp)
                                 (eai-code-agent--stats-record backend-name t)
                                 (funcall callback resp))
                                (t
                                 ;; Tool calls or other responses — treat as success
                                 (eai-code-agent--stats-record backend-name t)
                                 (funcall callback (format "%S" resp)))))))
                       (error
                        (eai-code-agent--stats-record backend-name nil)
                        (message "eai-code-agent: backend %s error: %S" backend-name err)
                        (try-next))))
                 (try-next))))))
      (try-next))))

;;; Sub-agent task wrapper

(declare-function gptel-agent--task "gptel-agent" (main-cb agent-type description prompt))

(defvar eai-code-agent--task-id-counter 0
  "Counter for generating unique sub-agent task IDs.")

(defun eai-code--agent-task (main-cb profile description prompt)
  "Run a sub-agent task with PROFILE, delegating to gptel-agent--task.

MAIN-CB is called with the result string.
PROFILE is a symbol naming an agent profile.
DESCRIPTION is a short task description.
PROMPT is the full prompt for the sub-agent.

Selects the first available backend from the profile and applies
it as a gptel preset before calling `gptel-agent--task'.
If no backend is available, MAIN-CB is called with an error string.

Tracks active sub-agents in the originating buffer's
`eai-code--sub-agents' alist and refreshes the *eai-code agents*
buffer.  When a sub-agent completes, its result is stored in the
agents buffer, not inserted into the chat buffer."
  (let ((backend-pair (eai-code-agent--select-backend profile))
        (orig-buf (current-buffer))
        (task-id (format "agent-%d" (cl-incf eai-code-agent--task-id-counter))))
    (if (not backend-pair)
        (funcall main-cb
                 (format "No available backend for profile '%s'" profile))
      (let* ((backend-name (car backend-pair))
             (model (cdr backend-pair))
             (preset `(:backend ,backend-name :model ,model)))
        ;; Track sub-agent start
        (when (buffer-live-p orig-buf)
          (with-current-buffer orig-buf
            (push (cons task-id
                        (list :description description
                              :profile profile
                              :backend backend-name
                              :model model
                              :status 'running
                              :start-time (float-time)))
                  eai-code--sub-agents)
            (eai-code--update-header-line)
            (eai-code-agent--refresh)))
        (gptel-with-preset preset
          (gptel-agent--task
           (lambda (result)
             (when (buffer-live-p orig-buf)
               (with-current-buffer orig-buf
                 (let ((entry (assoc task-id eai-code--sub-agents)))
                   (when entry
                     (setf (plist-get (cdr entry) :status) 'done)
                     (setf (plist-get (cdr entry) :end-time) (float-time))
                     (setf (plist-get (cdr entry) :result) result)
                     ;; Move to end of list (oldest first for display)
                     (setq eai-code--sub-agents
                           (append (delete entry eai-code--sub-agents)
                                   (list entry)))))
                 (eai-code--update-header-line)
                 (eai-code-agent--refresh)))
             (funcall main-cb result))
           profile description prompt))))))

;;; Interactive command

(defun eai-code-delegate-read-profile ()
  "Read a profile name interactively."
  (let ((profiles (eai-code-agent--profile-names)))
    (if (= (length profiles) 1)
        (car profiles)
      (intern (completing-read "Profile: " profiles nil t)))))

;;;###autoload
(defun eai-code-delegate (profile description prompt)
  "Delegate a task to a sub-agent with PROFILE.

DESCRIPTION is a short summary of the task.
PROMPT is the detailed instructions for the sub-agent.
Interactively, prompts for PROFILE and DESCRIPTION, then reads
PROMPT from the minibuffer."
  (interactive
   (list (eai-code-delegate-read-profile)
         (read-string "Task description: ")
         (read-string "Prompt: ")))
  (let ((buf (current-buffer)))
    (eai-code--agent-task
     (lambda (result)
       (with-current-buffer buf
         (message "Agent result: %s" result)))
     profile description prompt)
    (message "Delegated to profile '%s': %s" profile description)))

;;; Agent tool registration

;; The Agent tool is registered as an async tool so it can be called
;; by the main LLM.  It wraps `eai-code--agent-task'.

(defun eai-code-agent--tool (callback profile description prompt)
  "Agent tool for LLM delegation.

PROFILE is a symbol naming the agent profile to use.
DESCRIPTION is a short task description.
PROMPT is the detailed prompt.
Calls CALLBACK with the result string when the task completes."
  (eai-code--agent-task callback profile description prompt))

(defun eai-code-agent--tool-wrapper (callback profile-string description prompt)
  "Wrapper that interns PROFILE-STRING to a symbol before delegating."
  (eai-code-agent--tool callback (intern profile-string) description prompt))

;;;###autoload
(defun eai-code-agent-enable ()
  "Register the eai-code agent tool with gptel."
  (interactive)
  (if (not (fboundp 'gptel-make-tool))
      (message "eai-code-agent: gptel-make-tool not available, skipping")
    (when (and (boundp 'gptel-tools)
               (not (cl-some (lambda (tool)
                               (and (fboundp 'gptel-tool-name)
                                    (string= (gptel-tool-name tool) "eai_code_agent")))
                             gptel-tools)))
      (let ((tool (gptel-make-tool
                   :function #'eai-code-agent--tool-wrapper
                   :name "eai_code_agent"
                   :description "Delegate a task to a specialized sub-agent with its own profile and backend. Use this when a task can be parallelized, requires a different model capability (e.g., a cheaper model for summarization, a stronger model for complex reasoning), or should be isolated from the main conversation. The sub-agent runs independently and returns its result."
                   :args '((:name "profile"
                                  :type string
                                  :description "Agent profile name (e.g., 'summarizer', 'researcher', 'executor'). Must match a profile in eai-code-agent-profiles.")
                           (:name "description"
                                  :type string
                                  :description "Short task description, one sentence.")
                           (:name "prompt"
                                  :type string
                                  :description "Full detailed instructions for the sub-agent. Be specific about what it should do and what format the result should take."))
                   :async t)))
        (add-to-list 'gptel-tools tool))
      (message "eai-code agent tool enabled"))))

;;;###autoload
(defun eai-code-agent-disable ()
  "Unregister the eai-code agent tool from gptel."
  (interactive)
  (when (boundp 'gptel-tools)
    (setq gptel-tools
          (cl-remove-if (lambda (tool)
                          (and (fboundp 'gptel-tool-name)
                               (string= (gptel-tool-name tool) "eai_code_agent")))
                        gptel-tools)))
  (message "eai-code agent tool disabled"))

(provide 'eai-code-agent)
;;; eai-code-agent.el ends here
