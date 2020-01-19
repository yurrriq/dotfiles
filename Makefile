profile   ?= sruxps
notconfigs = Makefile README.org nix taskwarrior
configs    = $(filter-out ${notconfigs},$(wildcard *))


.PHONY: all ${configs}

all: ${configs} nix


.PHONY: nix taskwarrior

nix:
	@ stow -Rvt ~ $@

taskwarrior:
	$(error "taskwarrior configure is managed by home-manager now")


ifneq (,$(findstring B,$(MAKEFLAGS)))
${configs}::
	@ if [ -f $@/${profile}/Makefile ]; then \
		${MAKE} -C $@/${profile} ; \
	fi
endif


${configs}:: target=${HOME}

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


dry-build switch open-secrets close-secrets:
	@ ${MAKE} -C nix/.config/nixpkgs/machines/${profile} $@
