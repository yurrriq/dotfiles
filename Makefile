machine   ?= sruxps
nixos_dir ?= /etc/nixos


.PHONY: all
all: .stow-local-ignore
	@ stow -Rvt ${nixos_dir} .
	@ stow -Rvt ${nixos_dir} -d machines ${machine}


.PHONY: .envrc
.envrc:
	@ echo 'export profile=${profile}' >$@
	@ direnv allow


.PHONY: .stow-local-ignore
.stow-local-ignore:
	@ ls -A1 | sed '/^\(config\|modules\|overlays\)$$/d' >$@


.PHONY: close-secrets dry-build open-secrets switch
close-secrets dry-build open-secrets switch:
	@ ${MAKE} -C machines/${machine} $@
