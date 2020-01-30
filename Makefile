machine    ?= sruxps
nixos_dir  ?= /etc/nixos
cpif   ?= | cpif


ifneq (,$(findstring B,$(MAKEFLAGS)))
latexmk_flags = -gg
endif
latexmk_flags += -cd -shell-escape -xelatex


stow_flags := -R
ifneq (,$(findstring trace,$(MAKEFLAGS)))
stow_flags += -v
endif
stow       := stow ${stow_flags}


.PHONY: all
all: $(patsubst src/%.nw,%.nix,$(shell find src -name '*.nw')) docs/dotfiles.pdf


.PHONY: install
install: all
	@ cp -vr docs ${PREFIX}


docs/%.pdf: export TZ='America/Chicago'
docs/%.pdf: src/%.tex src/preamble.tex $(patsubst src/%.nw,src/%.tex,$(shell find src -name '*.nw'))
	@ mkdir -p $(@D)
	@ latexmk $(latexmk_flags) -outdir=../$(@D) $<


src/%.tex: src/%.nw
	noweave -n -index $^ ${cpif} $@


machines/%/hardware-configuration.nix: src/machines/%/hardware-configuration.nw
	notangle -R$@ $< ${cpif} $@


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
	@ ls -A1 | sed '/^\(config\|modules\|nix\|overlays\)$$/d' >$@


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


.PHONY: update
update: package ?= nixpkgs
update: sources := nix/sources.json
update: rev = $(shell jq -r '.["${package}"].rev[:8]' ${sources})
update: COMMIT_MSG_FILE = .git/COMMIT_EDITMSG
update:
	@ niv update ${package}
	@ git add ${sources}
	@ jq '"[nix/${package}]: ${rev} -> \(.["${package}"].rev[:8])"' \
	${sources} | xargs git commit -m
