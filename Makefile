EMACS=emacs

ALL: test

.PHONY: clean lisp tests

clean:
	@rm -f *.elc

%.elc: %.el
	@$(EMACS) -batch -Q -L deps -L . -f batch-byte-compile $<

compile: byte-compile

byte-compile:
	@$(EMACS) -Q -L deps -L . --batch -f batch-byte-compile *.el

lisp:
	@$(MAKE) -C lisp

test:
	@$(MAKE) -C t
