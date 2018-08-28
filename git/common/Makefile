.PHONY: all

all: README.org
	@ emacs --batch --quick -l ob-tangle --eval '(org-babel-tangle-file "$<")'
