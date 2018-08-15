.PHONY: all

all: README.org
	@ emacs --batch --quick \
		--load ob-tangle \
		--eval '(setq org-src-preserve-indentation t)' \
		--eval '(org-babel-tangle-file "$<")'
