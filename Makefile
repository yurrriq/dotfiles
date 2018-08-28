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


${configs}:: target=${HOME}

ifeq (nixps,${profile})
nix:: target=/etc/nixos
else ifneq (,$(findstring ${profile},hacktop srus))
nix:: target=${HOME}/.nixpkgs
endif

${configs}::
	@ if [ -d $@/${profile} ]; then \
		if [ -d $@/common ]; then \
			if [ -f $@/common/Makefile ]; then \
				${MAKE} -C $@/common ; \
			fi ; \
			stow -Rvt $@/${profile} common -d $@ ; \
		fi ; \
		stow -Rvt ${target} ${profile} -d $@ ; \
	fi


.envrc:
	@ echo 'export profile=${profile}' >$@
	@ direnv allow
