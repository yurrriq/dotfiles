profile   ?= srus
notconfigs = Makefile README.org
configs    = $(filter-out ${notconfigs},$(wildcard *))


.PHONY: all ${configs}

all: ${configs}


ifneq (,$(findstring B,$(MAKEFLAGS)))
${configs}::
	@ if [ -f $@/${profile}/Makefile ]; then \
		${MAKE} -C $@/${profile} ; \
	fi
endif


ifeq (nixps,${profile})
$(filter-out nix,${configs})::
	@ if [ -d $@/${profile} ]; then \
		stow -Rvt ${HOME} ${profile} -d $@ ; \
	fi
nix::
	@ if [ -d $@/${profile} ]; then \
		stow -Rvt /etc/nixos ${profile} -d $@ ; \
	fi
else
${configs}::
	@ if [ -d $@/${profile} ]; then \
		stow -Rvt ${HOME} ${profile} -d $@ ; \
	fi
endif


.envrc:
	@ echo 'export profile=${profile}' >$@
	@ direnv allow
