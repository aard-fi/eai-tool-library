;;; test-buffer.el --- Tests for eai-tool-library-buffer -*- lexical-binding: t; -*-
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
;; Tests for the eai-tool-library-buffer module.
;;
;;; Code:

(require 'test-init)
(require 'eai-tool-library-buffer)

(defmacro etl-buf--with-test-buffer (name initial-content &rest body)
  "Evaluate BODY with a named test buffer containing INITIAL-CONTENT.
The buffer is killed after BODY completes."
  (declare (indent 2))
  `(let ((buf (get-buffer-create ,name)))
     (unwind-protect
         (progn
           (with-current-buffer buf
             (erase-buffer)
             (insert ,initial-content))
           ,@body)
       (when (buffer-live-p buf)
         (kill-buffer buf)))))

;;; eai-tool-library-buffer--filename

(ert-deftest etl-buffer/filename/no-file ()
  "Returns nil for a buffer not visiting a file."
  (with-temp-buffer
    (should (null (eai-tool-library-buffer--filename (buffer-name))))))

(ert-deftest etl-buffer/filename/with-file ()
  "Returns the file name for a buffer visiting a file."
  (let* ((file (expand-file-name "test-data/lorem-ipsum.txt" gtl-test-directory))
         (buf (find-file-noselect file)))
    (unwind-protect
        (should (string-suffix-p "lorem-ipsum.txt"
                                 (eai-tool-library-buffer--filename (buffer-name buf))))
      (kill-buffer buf))))

;;; eai-tool-library-buffer--read-buffer-contents

(ert-deftest etl-buffer/read-contents/basic ()
  "Returns buffer contents as a string."
  (let ((eai-tool-library-max-result-size 200))
    (etl-buf--with-test-buffer "*etl-test-read*" "hello world"
      (should (string= "hello world"
                       (eai-tool-library-buffer--read-buffer-contents "*etl-test-read*"))))))

(ert-deftest etl-buffer/read-contents/empty-buffer ()
  "Returns empty string for an empty buffer."
  (let ((eai-tool-library-max-result-size 200))
    (etl-buf--with-test-buffer "*etl-test-read-empty*" ""
      (should (string= ""
                       (eai-tool-library-buffer--read-buffer-contents "*etl-test-read-empty*"))))))

(ert-deftest etl-buffer/read-contents/missing-buffer-errors ()
  "A name that is neither a file nor a live buffer errors and creates nothing."
  (let ((bufname "*etl-test-read-missing-xyz*"))
    (when (get-buffer bufname) (kill-buffer bufname))
    (should-error (eai-tool-library-buffer--read-buffer-contents bufname))
    (should-not (get-buffer bufname))))

(ert-deftest etl-buffer/read-contents/visits-file-path ()
  "A file path is visited instead of creating an empty non-file buffer."
  (let ((file (make-temp-file "etl-read-" nil ".txt" "file contents"))
        (eai-tool-library-max-result-size 200))
    (unwind-protect
        (should (equal (eai-tool-library-buffer--read-buffer-contents file)
                       "file contents"))
      (when-let* ((buf (find-buffer-visiting file)))
        (kill-buffer buf))
      (delete-file file))))

(ert-deftest etl-buffer/read-contents/over-limit ()
  "Returns limit-exceeded message when content exceeds max-result-size."
  (let ((eai-tool-library-max-result-size 5))
    (etl-buf--with-test-buffer "*etl-test-read-limit*" "hello world"
      (let ((result (eai-tool-library-buffer--read-buffer-contents "*etl-test-read-limit*")))
        (should (string-match-p "over" result))))))

;;; eai-tool-library-buffer--read-buffer-region

(ert-deftest etl-buffer/read-region/basic ()
  "Returns the specified region of the buffer."
  (let ((eai-tool-library-max-result-size 200))
    (etl-buf--with-test-buffer "*etl-test-region*" "hello world"
      (should (string= "hello"
                       (eai-tool-library-buffer--read-buffer-region "*etl-test-region*" 1 6))))))

(ert-deftest etl-buffer/read-region/full-buffer ()
  "Returns the full buffer content when region spans the whole buffer."
  (let ((eai-tool-library-max-result-size 200))
    (etl-buf--with-test-buffer "*etl-test-region-full*" "abcde"
      (should (string= "abcde"
                       (eai-tool-library-buffer--read-buffer-region
                        "*etl-test-region-full*" 1 6))))))

(ert-deftest etl-buffer/read-region/middle-section ()
  "Returns a middle section of the buffer."
  (let ((eai-tool-library-max-result-size 200))
    (etl-buf--with-test-buffer "*etl-test-region-mid*" "abcdefghij"
      (should (string= "cdef"
                       (eai-tool-library-buffer--read-buffer-region
                        "*etl-test-region-mid*" 3 7))))))

;;; eai-tool-library-buffer--read-buffer-contents-since-last-read

(ert-deftest etl-buffer/read-since-last/first-read-returns-all ()
  "First read returns the full buffer contents."
  (etl-buf--with-test-buffer "*etl-test-since*" "initial content"
    (let ((result (eai-tool-library-buffer--read-buffer-contents-since-last-read
                   "*etl-test-since*")))
      (should (string= "initial content" result)))))

(ert-deftest etl-buffer/read-since-last/second-read-returns-new ()
  "Second read returns only new content appended since first read."
  (etl-buf--with-test-buffer "*etl-test-since2*" "first"
    (eai-tool-library-buffer--read-buffer-contents-since-last-read "*etl-test-since2*")
    (with-current-buffer "*etl-test-since2*"
      (insert " second"))
    (let ((result (eai-tool-library-buffer--read-buffer-contents-since-last-read
                   "*etl-test-since2*")))
      (should (string= " second" result)))))

(ert-deftest etl-buffer/read-since-last/third-read-empty-when-no-change ()
  "Third read returns empty string when nothing new was appended."
  (etl-buf--with-test-buffer "*etl-test-since3*" "text"
    (eai-tool-library-buffer--read-buffer-contents-since-last-read "*etl-test-since3*")
    (eai-tool-library-buffer--read-buffer-contents-since-last-read "*etl-test-since3*")
    (let ((result (eai-tool-library-buffer--read-buffer-contents-since-last-read
                   "*etl-test-since3*")))
      (should (string= "" result)))))

;;; eai-tool-library-buffer--set-buffer-pos / --get-buffer-pos

(ert-deftest etl-buffer/get-buffer-pos/initial-returns-point-min ()
  "get-buffer-pos returns point-min on first call."
  (etl-buf--with-test-buffer "*etl-test-pos*" "hello world"
    (let ((pos (eai-tool-library-buffer--get-buffer-pos "*etl-test-pos*")))
      (should (= 1 pos)))))

(ert-deftest etl-buffer/set-get-pos/roundtrip ()
  "set-buffer-pos and get-buffer-pos roundtrip correctly."
  (etl-buf--with-test-buffer "*etl-test-setpos*" "hello world"
    (eai-tool-library-buffer--set-buffer-pos "*etl-test-setpos*" 5)
    (should (= 5 (eai-tool-library-buffer--get-buffer-pos "*etl-test-setpos*")))))

;;; eai-tool-library-buffer--list-buffers

(ert-deftest etl-buffer/list-buffers/returns-string ()
  "list-buffers returns a non-empty string."
  (let ((result (eai-tool-library-buffer--list-buffers)))
    (should (stringp result))
    (should (> (length result) 0))))

;;; eai-tool-library-buffer--get-in-direction

(ert-deftest etl-buffer/get-in-direction/no-window ()
  "Returns nil when there is no window in the given direction."
  (should (null (eai-tool-library-buffer--get-in-direction "right")))
  (should (null (eai-tool-library-buffer--get-in-direction "left")))
  (should (null (eai-tool-library-buffer--get-in-direction "above")))
  (should (null (eai-tool-library-buffer--get-in-direction "below"))))

(ert-deftest etl-buffer/get-in-direction/accepts-symbol ()
  "Accepts a symbol direction without error."
  (should-not (eai-tool-library-buffer--get-in-direction 'right)))

;;; eai-tool-library-buffer--erase-buffer

(ert-deftest etl-buffer/erase-buffer/clears-content ()
  "Erases the buffer content."
  (etl-buf--with-test-buffer "*etl-test-erase*" "some content to erase"
    (eai-tool-library-buffer--erase-buffer "*etl-test-erase*")
    (with-current-buffer "*etl-test-erase*"
      (should (= 0 (buffer-size))))))

(ert-deftest etl-buffer/erase-buffer/empty-stays-empty ()
  "Erasing an already empty buffer is a no-op."
  (etl-buf--with-test-buffer "*etl-test-erase-empty*" ""
    (eai-tool-library-buffer--erase-buffer "*etl-test-erase-empty*")
    (with-current-buffer "*etl-test-erase-empty*"
      (should (= 0 (buffer-size))))))

;;; eai-tool-library-buffer--buffer-size

(ert-deftest etl-buffer/buffer-size/empty ()
  "Returns 0 for an empty buffer."
  (etl-buf--with-test-buffer "*etl-test-size-empty*" ""
    (should (= 0 (eai-tool-library-buffer--buffer-size "*etl-test-size-empty*")))))

(ert-deftest etl-buffer/buffer-size/non-empty ()
  "Returns the correct size for a non-empty buffer."
  (let ((eai-tool-library-max-result-size 200))
    (etl-buf--with-test-buffer "*etl-test-size*" "hello"
      (should (= 5 (eai-tool-library-buffer--buffer-size "*etl-test-size*"))))))

;;; eai-tool-library-buffer--replace-region

(ert-deftest etl-buffer/replace-region/basic ()
  "Replaces the specified region with new text."
  (etl-buf--with-test-buffer "*etl-test-replace*" "hello world"
    (eai-tool-library-buffer--replace-region "*etl-test-replace*" 1 6 "goodbye")
    (with-current-buffer "*etl-test-replace*"
      (should (string= "goodbye world" (buffer-string))))))

(ert-deftest etl-buffer/replace-region/at-end ()
  "Replaces text at the end of the buffer."
  (etl-buf--with-test-buffer "*etl-test-replace-end*" "hello world"
    (eai-tool-library-buffer--replace-region "*etl-test-replace-end*" 7 12 "emacs")
    (with-current-buffer "*etl-test-replace-end*"
      (should (string= "hello emacs" (buffer-string))))))

(ert-deftest etl-buffer/replace-region/with-longer-text ()
  "Replacing with longer text grows the buffer."
  (etl-buf--with-test-buffer "*etl-test-replace-grow*" "hi"
    (eai-tool-library-buffer--replace-region "*etl-test-replace-grow*" 1 3 "hello world")
    (with-current-buffer "*etl-test-replace-grow*"
      (should (string= "hello world" (buffer-string))))))

;;; eai-tool-library-buffer--remove-region

(ert-deftest etl-buffer/remove-region/basic ()
  "Removes the specified region."
  (etl-buf--with-test-buffer "*etl-test-remove*" "hello world"
    (eai-tool-library-buffer--remove-region "*etl-test-remove*" 1 7)
    (with-current-buffer "*etl-test-remove*"
      (should (string= "world" (buffer-string))))))

(ert-deftest etl-buffer/remove-region/middle ()
  "Removes a middle section."
  (etl-buf--with-test-buffer "*etl-test-remove-mid*" "hello world"
    (eai-tool-library-buffer--remove-region "*etl-test-remove-mid*" 6 7)
    (with-current-buffer "*etl-test-remove-mid*"
      (should (string= "helloworld" (buffer-string))))))

;;; eai-tool-library-buffer--insert-at

(ert-deftest etl-buffer/insert-at/beginning ()
  "Inserts text at position 0 (before all content)."
  (etl-buf--with-test-buffer "*etl-test-insert*" "world"
    (eai-tool-library-buffer--insert-at "*etl-test-insert*" 0 "hello ")
    (with-current-buffer "*etl-test-insert*"
      (should (string= "hello world" (buffer-string))))))

(ert-deftest etl-buffer/insert-at/middle ()
  "Inserts text in the middle of the buffer."
  (etl-buf--with-test-buffer "*etl-test-insert-mid*" "helloworld"
    (eai-tool-library-buffer--insert-at "*etl-test-insert-mid*" 5 " ")
    (with-current-buffer "*etl-test-insert-mid*"
      (should (string= "hello world" (buffer-string))))))

(provide 'test-buffer)
;;; test-buffer.el ends here
