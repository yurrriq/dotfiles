PATH := $(PWD)/bin:$(PATH)

machine   ?= sruxps
nixos_dir ?= /etc/nixos
cpif      ?= | cpif


ifneq (,$(findstring B,$(MAKEFLAGS)))
latexmk_flags  = -gg
endif


stow_flags := -R
ifneq (,$(findstring trace,$(MAKEFLAGS)))
stow_flags += -v
endif
stow       := $(shell type -p stow) ${stow_flags}


NW_SRCS := $(shell find src -name '*.nw')

WEIRD_SRCS := \
pkgs/development/node-packages/node-packages.json \
pkgs/shells/fish/kubectl-completions/default.nix
SRCS := $(filter-out ${WEIRD_SRCS}, $(shell awk '/<<[^ *]+\.\w+>>=$$/{ gsub(/(<<|>>=)/, ""); print $$0 }' ${NW_SRCS} | sort -u))
NIX_SRCS := $(filter %.nix, ${SRCS}) pkgs/development/node-packages/node-packages.nix
SH_SRCS := $(filter %.sh, ${SRCS})
OTHER_SRCS := $(filter-out ${NIX_SRCS} ${SH_SRCS} ${TEX_SRCS}, ${SRCS})
TEX_SRCS := $(patsubst src/%.nw,src/%.tex,${NW_SRCS})

DEFS := $(patsubst src/%.nw,src/%.defs,${NW_SRCS})


.PHONY: all
all: generate-config srcs docs/dotfiles.pdf


.PHONY: srcs
srcs: nix-srcs ${SH_SRCS} ${OTHER_SRCS} ${WEIRD_SRCS}


.PHONY: nix-srcs
nix-srcs: ${NIX_SRCS}


.PHONY: nixpkgs-fmt
nixpkgs-fmt: nix-srcs
	nixpkgs-fmt ${NIX_SRCS}


.PHONY: tex
tex: ${TEX_SRCS}


.PHONY: clean
clean:
	@ rm -f ${TEX_SRCS} ${DEFS} src/all.defs src/dotfiles.nwi
	@ latexmk -c -f docs/dotfiles.pdf
	@ rm -fr docs/_minted-dotfiles


.PHONY: clobber
clobber:
	@ rm -fr docs


.PHONY: install
install: all clean
	@ cp -vr docs/* ${PREFIX}
	@ echo "theme: jekyll-theme-hacker" >${PREFIX}/_config.yml
	@ echo "[PDF](./dotfiles.pdf)" >${PREFIX}/index.md


docs/%.pdf: export TZ='America/Chicago'
docs/%.pdf: src/%.tex src/preamble.tex src/glossary.tex src/%.bib ${TEX_SRCS}
	@ mkdir -p $(@D)
	latexmk $(latexmk_flags) -cd -f -outdir=$(CURDIR)/$(@D) -pdf $<
	@ noindex src/dotfiles
	latexmk $(latexmk_flags) -cd -f -outdir=$(CURDIR)/$(@D) -pdf $<


src/%.defs: src/%.nw
	@ nodefs $< >$@


src/all.defs: ${DEFS}
	@ sort -u $^ ${cpif} $@


# .INTERMEDIATE: ${TEX_SRCS}
src/%.tex: src/%.nw src/all.defs
	@ noweave -delay -indexfrom src/all.defs -latex -n -filter fix-underscores $^ ${cpif} $@


pkgs/development/node-packages/node-packages.json: src/packages.nw
	@ mkdir -p $(@D)
	@ notangle -R$@ $< ${cpif} $@


pkgs/development/node-packages/node-packages.nix: pkgs/development/node-packages/node-packages.json
	@ ${MAKE} -C $(@D)


pkgs/shells/fish/kubectl-completions/default.nix: src/packages.nw
	@ mkdir -p $(@D)
	@ notangle -R$@ $< ${cpif} $@


${SRCS}::
	@ mkdir -p $(@D)
	@ notangle -R$@ src/$(basename $@).nw ${cpif} $@


${SH_SRCS}::
	chmod a+x $@


.PHONY: .stow-local-ignore
.stow-local-ignore:
	@ ls -A1 | sed '/^\(config\|flake\.\(nix\|lock\)\|modules\|nix\|overlays\|pkgs\)$$/d' >$@


%: %.enc
	@ sops -d $< >${@:.enc=}


.PHONY: build diff dry-build switch test

build dry-build: stow
	@ nixos-rebuild --option pure-eval false $@

switch test: stow
	@ sudo nixos-rebuild --option pure-eval false $@

diff: build
	@ nix store diff-closures /run/current-system ./result


.PHONY: build-hm switch-hm xsessions

switch-hm: cachix xsessions

build-hm switch-hm:
	@ home-manager --impure --flake .#ebailey ${@:-hm=}

xsessions:
	@ sudo ${stow} -t /usr/share/$@ $@


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


.PHONY: generate-config
generate-config: machines/${machine}/hardware-configuration.nix

machines/${machine}/hardware-configuration.nix:
	nixos-generate-config --root ${PWD} --dir $(@D)
	nixpkgs-fmt $@


part ?= patch
.PHONY: bump-version
bump-version:
	semver bump ${part} $(file <VERSION) | tr -d '\n' >VERSION
