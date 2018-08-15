profile   ?= srus
notconfigs = Makefile README.org
configs    = $(filter-out ${notconfigs},$(wildcard *))


.PHONY: all ${configs}

all: ${configs}


${configs}:
ifneq (,$(findstring B,$(MAKEFLAGS)))
		if [ -f $@/${profile}/Makefile ]; then \
			${MAKE} -C $@/${profile} ; \
		fi
endif
	@ if [ -d $@/${profile} ]; then \
		stow -Rvt ${HOME} ${profile} -d $@ ; \
	fi
