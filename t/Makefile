ALL: module-tests

.PHONY: module-tests test coverage

TESTS =
TESTS += test-core.el
TESTS += test-buffer.el
TESTS += test-date-time.el
TESTS += test-elisp.el
TESTS += test-emacs.el
TESTS += test-os.el
TESTS += test-search-and-replace.el
TESTS += test-outline.el
TESTS += test-project.el

module-tests: $(TESTS)
	@for test in $^; do\
	  echo ">> Running tests from $$test";\
	  emacs -batch -Q --eval "(progn \
	    (load-file \"test-init.el\")\
	    (load-file \"$$test\")\
	    (ert-run-tests-batch-and-exit))";\
	  if [ $$? -ne 0 ]; then exit 1; fi ;\
	done

coverage: coverage-runner.el
	emacs -batch -Q --eval "(load-file \"coverage-runner.el\")"
