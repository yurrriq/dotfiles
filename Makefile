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
stow       := stow ${stow_flags}


NW_SRCS := $(shell find src -name '*.nw')

SRCS := $(shell awk '/<<[^ *]+\.\w+>>=$$/{ gsub(/(<<|>>=)/, ""); print $$0 }' ${NW_SRCS} | sort -u)
NIX_SRCS := $(filter %.nix, ${SRCS})
SH_SRCS := $(filter %.sh, ${SRCS})
OTHER_SRCS := $(filter-out ${NIX_SRCS} ${SH_SRCS} ${TEX_SRCS}, ${SRCS})
TEX_SRCS := $(patsubst src/%.nw,src/%.tex,${NW_SRCS})

DEFS := $(patsubst src/%.nw,src/%.defs,${NW_SRCS})


.PHONY: all
all: generate-config srcs docs/dotfiles.pdf


.PHONY: srcs
srcs: nix-srcs ${SH_SRCS} ${OTHER_SRCS}


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
	nixos-generate-config --root ${PWD} --dir /$(@D)
	nixpkgs-fmt $@
