;;; coverage-runner.el --- Coverage reporting for eai-tool-library tests -*- lexical-binding: t; -*-
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
;; Instruments source files with testcover, runs all ERT tests in one session,
;; and reports per-file and aggregate coverage.
;;
;; Run via: emacs -batch -Q --eval "(load-file \"t/coverage-runner.el\")"
;; from the project root.
;;
;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'testcover)
(require 'edebug)

;;; Paths

(defvar etl-cov--root
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Directory this file lives in (the t/ directory).")

(defvar etl-cov--project-root
  (expand-file-name ".." etl-cov--root)
  "Project root (parent of t/).")

(defvar etl-cov--source-files
  (mapcar (lambda (f) (expand-file-name f etl-cov--project-root))
          '("eai-tool-library.el"
            "eai-tool-library-buffer.el"
            "eai-tool-library-date-time.el"
            "eai-tool-library-elisp.el"
            "eai-tool-library-emacs.el"
            "eai-tool-library-os.el"
            "eai-tool-library-outline.el"
            "eai-tool-library-project.el"
            "eai-tool-library-search-and-replace.el"))
  "Source files to instrument and measure coverage for.")

(defvar etl-cov--test-files
  (mapcar (lambda (f) (expand-file-name f etl-cov--root))
          '("test-init.el"
            "test-core.el"
            "test-buffer.el"
            "test-date-time.el"
            "test-elisp.el"
            "test-emacs.el"
            "test-os.el"
            "test-outline.el"
            "test-project.el"
            "test-search-and-replace.el"))
  "Test files to load and run.")

;;; Instrumentation tracking

(defvar etl-cov--file-symbols (make-hash-table :test 'equal)
  "Maps source file paths to lists of instrumented function symbols.")

(defun etl-cov--edebug-symbols ()
  "Return all symbols currently having an edebug-coverage property."
  (let (syms)
    (mapatoms (lambda (sym)
                (when (get sym 'edebug-coverage)
                  (push sym syms))))
    syms))

(defun etl-cov--instrument-file (file)
  "Instrument FILE with testcover, recording which symbols it defines."
  (let ((before (etl-cov--edebug-symbols)))
    (condition-case err
        (testcover-start file)
      (error (message "WARNING: testcover-start failed for %s: %s"
                      (file-name-nondirectory file) err)))
    (let* ((after (etl-cov--edebug-symbols))
           (new (cl-set-difference after before)))
      (puthash file new etl-cov--file-symbols)
      (length new))))

;;; Coverage analysis

(defun etl-cov--form-covered-p (state)
  "Return non-nil if coverage STATE means the form was executed."
  (not (eq state 'edebug-unknown)))

(defun etl-cov--function-stats (sym)
  "Return (TOTAL . COVERED) form counts for SYM, or nil if not instrumented."
  (let ((vec (get sym 'edebug-coverage)))
    (when vec
      (let ((total (length vec))
            (covered 0))
        (dotimes (i total)
          (when (etl-cov--form-covered-p (aref vec i))
            (cl-incf covered)))
        (cons total covered)))))

(defun etl-cov--function-called-p (sym)
  "Return non-nil if SYM was called at least once during testing."
  (let ((stats (etl-cov--function-stats sym)))
    (and stats (> (cdr stats) 0))))

;;; Reporting

(defun etl-cov--pct (covered total)
  "Return coverage percentage string."
  (if (zerop total)
      "N/A"
    (format "%.1f%%" (* 100.0 (/ (float covered) total)))))

(defun etl-cov--print-file-report (file)
  "Print coverage report for FILE."
  (let* ((syms (gethash file etl-cov--file-symbols))
         (name (file-name-nondirectory file))
         (fn-total (length syms))
         (fn-covered (cl-count-if #'etl-cov--function-called-p syms))
         (form-total 0)
         (form-covered 0)
         uncovered)
    (dolist (sym syms)
      (let ((stats (etl-cov--function-stats sym)))
        (when stats
          (cl-incf form-total (car stats))
          (cl-incf form-covered (cdr stats))))
      (unless (etl-cov--function-called-p sym)
        (push sym uncovered)))
    (message "")
    (message "  %s" name)
    (message "    Functions : %d/%d (%s)"
             fn-covered fn-total (etl-cov--pct fn-covered fn-total))
    (message "    Forms     : %d/%d (%s)"
             form-covered form-total (etl-cov--pct form-covered form-total))
    (when uncovered
      (message "    Uncovered :")
      (dolist (sym (sort uncovered (lambda (a b)
                                     (string< (symbol-name a) (symbol-name b)))))
        (message "      - %s" (symbol-name sym))))
    (list fn-total fn-covered form-total form-covered)))

(defun etl-cov--print-report ()
  "Print the full coverage report to stderr."
  (message "\n========== COVERAGE REPORT ==========")
  (let ((grand-fn-total 0)
        (grand-fn-covered 0)
        (grand-form-total 0)
        (grand-form-covered 0))
    (dolist (file etl-cov--source-files)
      (cl-destructuring-bind (ft fc fot foc)
          (etl-cov--print-file-report file)
        (cl-incf grand-fn-total ft)
        (cl-incf grand-fn-covered fc)
        (cl-incf grand-form-total fot)
        (cl-incf grand-form-covered foc)))
    (message "")
    (message "  TOTAL")
    (message "    Functions : %d/%d (%s)"
             grand-fn-covered grand-fn-total
             (etl-cov--pct grand-fn-covered grand-fn-total))
    (message "    Forms     : %d/%d (%s)"
             grand-form-covered grand-form-total
             (etl-cov--pct grand-form-covered grand-form-total))
    (message "======================================")))

;;; Main entry point

(defun etl-cov--run ()
  "Instrument source files, run tests, and report coverage."
  ;; Set up load path
  (add-to-list 'load-path etl-cov--project-root)
  (add-to-list 'load-path etl-cov--root)

  ;; Step 1: Instrument source files
  (message "Instrumenting source files...")
  (dolist (file etl-cov--source-files)
    (let ((n (etl-cov--instrument-file file)))
      (message "  %s: %d function(s)" (file-name-nondirectory file) n)))

  ;; Step 2: Load test files (suppress messages from test definitions)
  (message "\nLoading test files...")
  (dolist (file etl-cov--test-files)
    (condition-case err
        (load-file file)
      (error (message "WARNING: Failed to load %s: %s"
                      (file-name-nondirectory file) err))))

  ;; Step 3: Run all ERT tests
  (message "\nRunning tests...")
  (let ((results (ert-run-tests-batch t)))
    (message "\nTest results: %d passed, %d failed, %d skipped"
             (ert-stats-completed-expected results)
             (ert-stats-completed-unexpected results)
             (ert-stats-skipped results))

    ;; Step 4: Print coverage report
    (etl-cov--print-report)

    ;; Exit with non-zero if tests failed
    (when (> (ert-stats-completed-unexpected results) 0)
      (kill-emacs 1))))

(etl-cov--run)

(provide 'coverage-runner)
;;; coverage-runner.el ends here
