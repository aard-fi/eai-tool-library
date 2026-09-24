;;; test-core.el --- Tests for eai-tool-library core -*- lexical-binding: t; -*-
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
;; Tests for the eai-tool-library core module.
;;
;;; Code:

(require 'test-init)
(require 'eai-tool-library)

;;; eai-tool-library--limit-result

(ert-deftest etl-core/limit-result/small ()
  "Small result passes through unchanged."
  (let ((eai-tool-library-max-result-size 100))
    (should (equal "hello" (eai-tool-library--limit-result "hello")))))

(ert-deftest etl-core/limit-result/exactly-under-limit ()
  "Result shorter than limit passes through."
  (let* ((eai-tool-library-max-result-size 10)
         (s (make-string 9 ?x)))
    (should (equal s (eai-tool-library--limit-result s)))))

(ert-deftest etl-core/limit-result/at-limit ()
  "Result at exactly the limit triggers the error message."
  (let* ((eai-tool-library-max-result-size 5)
         (s (make-string 5 ?x))
         (result (eai-tool-library--limit-result s)))
    (should (stringp result))
    (should (string-match-p "over" result))))

(ert-deftest etl-core/limit-result/over-limit ()
  "Result over limit returns the error string."
  (let ((eai-tool-library-max-result-size 5))
    (let ((result (eai-tool-library--limit-result "hello world")))
      (should (stringp result))
      (should (string-match-p "over" result))
      (should (string-match-p "5" result)))))

(ert-deftest etl-core/limit-result/non-string-small ()
  "Non-string result is passed through when its representation is small."
  (let ((eai-tool-library-max-result-size 100))
    (should (equal 42 (eai-tool-library--limit-result 42)))))

(ert-deftest etl-core/limit-result/list-small ()
  "List result passes through when small."
  (let ((eai-tool-library-max-result-size 100))
    (should (equal '(1 2 3) (eai-tool-library--limit-result '(1 2 3))))))

;;; eai-tool-library-append-tools

(defvar etl--test-append-var nil)

(ert-deftest etl-core/append-tools/to-empty ()
  "Append to an empty list."
  (setq etl--test-append-var nil)
  (eai-tool-library-append-tools 'etl--test-append-var '(a b c))
  (should (equal '(a b c) etl--test-append-var))
  (setq etl--test-append-var nil))

(ert-deftest etl-core/append-tools/to-non-empty ()
  "Append to a non-empty list."
  (setq etl--test-append-var '(a b))
  (eai-tool-library-append-tools 'etl--test-append-var '(c d))
  (should (equal '(a b c d) etl--test-append-var))
  (setq etl--test-append-var nil))

(ert-deftest etl-core/append-tools/append-empty ()
  "Appending an empty list leaves the variable unchanged."
  (setq etl--test-append-var '(a b))
  (eai-tool-library-append-tools 'etl--test-append-var '())
  (should (equal '(a b) etl--test-append-var))
  (setq etl--test-append-var nil))

(ert-deftest etl-core/append-tools/multiple-calls ()
  "Multiple appends accumulate correctly."
  (setq etl--test-append-var nil)
  (eai-tool-library-append-tools 'etl--test-append-var '(1))
  (eai-tool-library-append-tools 'etl--test-append-var '(2))
  (eai-tool-library-append-tools 'etl--test-append-var '(3))
  (should (equal '(1 2 3) etl--test-append-var))
  (setq etl--test-append-var nil))

;;; eai-tool-library--debug-log

(ert-deftest etl-core/debug-log/creates-buffer ()
  "Debug log creates the debug buffer when it does not exist."
  (let ((eai-tool-library-debug-buffer "*etl-test-debug-log-create*"))
    (when (get-buffer eai-tool-library-debug-buffer)
      (kill-buffer eai-tool-library-debug-buffer))
    (unwind-protect
        (progn
          (eai-tool-library--debug-log "create-test")
          (should (get-buffer eai-tool-library-debug-buffer)))
      (when (get-buffer eai-tool-library-debug-buffer)
        (kill-buffer eai-tool-library-debug-buffer)))))

(ert-deftest etl-core/debug-log/inserts-message ()
  "Debug log inserts the message into the buffer."
  (let ((eai-tool-library-debug-buffer "*etl-test-debug-log-msg*"))
    (when (get-buffer eai-tool-library-debug-buffer)
      (kill-buffer eai-tool-library-debug-buffer))
    (unwind-protect
        (progn
          (eai-tool-library--debug-log "etl-unique-sentinel-msg-xyz")
          (with-current-buffer eai-tool-library-debug-buffer
            (should (string-match-p "etl-unique-sentinel-msg-xyz"
                                    (buffer-string)))))
      (when (get-buffer eai-tool-library-debug-buffer)
        (kill-buffer eai-tool-library-debug-buffer)))))

(ert-deftest etl-core/debug-log/appends-newline ()
  "Debug log appends a newline after each message."
  (let ((eai-tool-library-debug-buffer "*etl-test-debug-log-nl*"))
    (when (get-buffer eai-tool-library-debug-buffer)
      (kill-buffer eai-tool-library-debug-buffer))
    (unwind-protect
        (progn
          (eai-tool-library--debug-log "line-one")
          (eai-tool-library--debug-log "line-two")
          (with-current-buffer eai-tool-library-debug-buffer
            (should (string-match-p "line-one\nline-two" (buffer-string)))))
      (when (get-buffer eai-tool-library-debug-buffer)
        (kill-buffer eai-tool-library-debug-buffer)))))

;;; eai-tool-library--get-buffer

(ert-deftest etl-core/get-buffer/buffer-object ()
  "Passing a buffer object returns it unchanged."
  (with-temp-buffer
    (should (eq (current-buffer)
                (eai-tool-library--get-buffer (current-buffer))))))

(ert-deftest etl-core/get-buffer/suffix-match ()
  "A buffer is found by a trailing path segment of its file name."
  (let* ((file (expand-file-name "test-data/lorem-ipsum.txt" gtl-test-directory))
         (buf (find-file-noselect file)))
    (unwind-protect
        (should (eq buf (eai-tool-library--get-buffer "test-data/lorem-ipsum.txt")))
      (kill-buffer buf))))

(ert-deftest etl-core/get-buffer/existing-file ()
  "Passing an existing absolute file path returns a buffer visiting it."
  (let* ((file (expand-file-name "test-data/lorem-ipsum.txt" gtl-test-directory))
         (buf (eai-tool-library--get-buffer file)))
    (unwind-protect
        (progn
          (should (bufferp buf))
          (should (buffer-live-p buf)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest etl-core/get-buffer/basename-match ()
  "A buffer visiting a file is found by its basename."
  (let* ((file (expand-file-name "test-data/lorem-ipsum.txt" gtl-test-directory))
         (buf (find-file-noselect file)))
    (unwind-protect
        (should (eq buf (eai-tool-library--get-buffer "lorem-ipsum.txt")))
      (kill-buffer buf))))

(ert-deftest etl-core/get-buffer/nonexistent-errors ()
  "Passing a nonexistent absolute path without create-p signals an error."
  (should-error
   (eai-tool-library--get-buffer "/tmp/etl-nonexistent-zzz-99999.el")
   :type 'error))

(ert-deftest etl-core/get-buffer/create-p-for-missing ()
  "Passing create-p creates a buffer for a nonexistent file without error."
  (let* ((tmpfile "/tmp/etl-test-create-p-zzz.el")
         (buf (eai-tool-library--get-buffer tmpfile t)))
    (unwind-protect
        (progn
          (should (bufferp buf))
          (should (buffer-live-p buf)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

;;; eai-tool-library-write-action

(ert-deftest etl-core/write-action/nil-file ()
  "A nil file (buffer without file) always returns ask."
  (should (eq 'ask (eai-tool-library-write-action nil))))

(ert-deftest etl-core/write-action/outside-project-default ()
  "Files outside the project default to eai-tool-library-write-outside-project."
  (let ((eai-tool-library-write-policy nil)
        (eai-tool-library-write-outside-project 'deny))
    (should (eq 'deny (eai-tool-library-write-action "/tmp/etl-write-policy-test.txt")))))

(ert-deftest etl-core/write-action/explicit-policy-wins ()
  "A matching explicit policy entry overrides defaults."
  (let ((eai-tool-library-write-policy '(("/tmp" . allow)))
        (eai-tool-library-write-outside-project 'deny))
    (should (eq 'allow (eai-tool-library-write-action "/tmp/etl-write-policy-test.txt")))))

(ert-deftest etl-core/write-action/longest-policy-wins ()
  "The longest matching directory in the policy wins."
  (let ((eai-tool-library-write-policy '(("/" . deny)
                                         ("/tmp" . allow))))
    (should (eq 'allow (eai-tool-library-write-action "/tmp/etl-write-policy-test.txt")))))

;;; eai-tool-library-write-confirm-p

(ert-deftest etl-core/write-confirm-p/ask-returns-t ()
  "Returns non-nil when the action is ask."
  (let ((eai-tool-library-write-policy '(("/tmp" . ask))))
    (should (eai-tool-library-write-confirm-p "/tmp/test.txt"))))

(ert-deftest etl-core/write-confirm-p/allow-returns-nil ()
  "Returns nil when the action is allow."
  (let ((eai-tool-library-write-policy '(("/tmp" . allow))))
    (should-not (eai-tool-library-write-confirm-p "/tmp/test.txt"))))

(ert-deftest etl-core/write-confirm-p/deny-returns-nil ()
  "Returns nil when the action is deny."
  (let ((eai-tool-library-write-policy '(("/tmp" . deny))))
    (should-not (eai-tool-library-write-confirm-p "/tmp/test.txt"))))

;;; eai-tool-library-write-deny-check

(ert-deftest etl-core/write-deny-check/deny-signals-error ()
  "Signals an error when the action is deny."
  (let ((eai-tool-library-write-policy '(("/tmp" . deny))))
    (should-error
     (eai-tool-library-write-deny-check "/tmp/test.txt")
     :type 'error)))

(ert-deftest etl-core/write-deny-check/allow-ok ()
  "Does nothing when the action is allow."
  (let ((eai-tool-library-write-policy '(("/tmp" . allow))))
    (eai-tool-library-write-deny-check "/tmp/test.txt")))

(ert-deftest etl-core/write-deny-check/ask-ok ()
  "Does nothing when the action is ask."
  (let ((eai-tool-library-write-policy '(("/tmp" . ask))))
    (eai-tool-library-write-deny-check "/tmp/test.txt")))

(provide 'test-core)
;;; test-core.el ends here
