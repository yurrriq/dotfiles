machine    ?= sruxps
nixos_dir  ?= /etc/nixos
stow_flags := -R
ifneq (,$(findstring trace,$(MAKEFLAGS)))
stow_flags += -v
endif
stow       := stow ${stow_flags}


.DEFAULT_GOAL := switch


.PHONY: .envrc
.envrc:
	$(file >$@,${envrc_text})
	@ direnv allow

define envrc_text
export machine=${machine}
eval "$$(lorri direnv)"
endef


.PHONY: .stow-local-ignore
.stow-local-ignore:
	@ ls -A1 | sed '/^\(config\|modules\|overlays\)$$/d' >$@


%: %.enc
	@ sops -d $< >${@:.enc=}


.PHONY: build dry-build switch
build dry-build switch: stow
	@ sudo nixos-rebuild $@


.PHONY: cachix
cachix: cachix/cachix.dhall
	@ mkdir -p ~/.config/$@ $@
	@ ${stow} -t ~/.config/$@ $@


.PHONY: secrets
secrets: $(patsubst %.enc,%,$(wildcard machines/${machine}/secrets/*.enc))
	@ sudo mkdir -p ${nixos_dir}/$@
	@ sudo ${stow} -t ${nixos_dir}/$@ -d machines/${machine} $@


.PHONY: stow
stow: .stow-local-ignore cachix secrets
	@ sudo ${stow} -t ${nixos_dir} .
	@ sudo ${stow} -t ${nixos_dir} -d machines ${machine}
