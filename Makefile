profile ?= srus

configs = \
	emacs \
	git \
	nix


.PHONY: all ${configs}

all: ${configs}

${configs}:
	@ ${MAKE} -C $@/${profile}
	stow -Rvt ${HOME} ${profile} -d $@
