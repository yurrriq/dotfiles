PATH           := $(PWD)/bin:$(PATH)
cpif           ?= | cpif
kernel_version := $(uname -v)


ifneq (,$(findstring B,$(MAKEFLAGS)))
latexmk_flags  = -gg
endif


stow_flags := -R
ifneq (,$(findstring trace,$(MAKEFLAGS)))
stow_flags += -v
endif
stow       := stow ${stow_flags}


NW_SRCS := $(shell find src -name '*.nw')

WEIRD_SRCS := \
default.nix \
pkgs/development/node-packages/node-packages.json
SRCS := $(filter-out ${WEIRD_SRCS}, $(shell awk '/<<[^ *]+\.\w+>>=$$/{ gsub(/(<<|>>=)/, ""); print $$0 }' ${NW_SRCS} | sort -u))
NIX_SRCS := $(filter %.nix, ${SRCS}) # pkgs/development/node-packages/node-packages.nix
SH_SRCS := $(filter %.sh, ${SRCS})
OTHER_SRCS := $(filter-out ${NIX_SRCS} ${SH_SRCS} ${TEX_SRCS}, ${SRCS})
TEX_SRCS := $(patsubst src/%.nw,src/%.tex,${NW_SRCS})


.PHONY: all
all:: srcs docs/dotfiles.pdf


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
	@ rm -f ${TEX_SRCS} src/dotfiles.nwi
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


# .INTERMEDIATE: ${TEX_SRCS}
src/%.tex: src/%.nw
	@ noweave -delay -latex -n -filter fix-underscores $^ ${cpif} $@


pkgs/development/node-packages/node-packages.json: src/packages.nw
	@ mkdir -p $(@D)
	@ notangle -R$@ $< ${cpif} $@


pkgs/development/node-packages/node-packages.nix: pkgs/development/node-packages/node-packages.json
	@ ${MAKE} -C $(@D)


default.nix: src/packages.nw
	@ notangle -R$@ $< ${cpif} $@


${SRCS}::
	@ mkdir -p $(@D)
	@ notangle -R$@ src/$(basename $@).nw ${cpif} $@


${SH_SRCS}::
	chmod a+x $@


.PHONY: .stow-local-ignore
.stow-local-ignore:
	@ ls -A1 | sed '/^\(config\|flake\.\(nix\|lock\)\|modules\|nix\|overlays\|pkgs\)$$/d' >$@

stow:: .stow-local-ignore cachix


%: %.enc
	@ sops -d $< >${@:.enc=}


.PHONY: build diff dry-build stow switch test

ifneq (,$(findstring NixOS,${kernel_version}))
include Makefile.nixos
else
include Makefile.home-manager
endif

ifeq (,$(findstring Ubuntu,${kernel_version}))
include Makefile.ubuntu
endif


.PHONY: bump-version
bump-version: part ?= patch
bump-version:
	semver bump ${part} $(file <VERSION) | tr -d '\n' >VERSION


.PHONY: cachix
cachix: cachix/cachix.dhall
	@ mkdir -p ~/.config/$@ $@
	@ ${stow} -t ~/.config/$@ $@


stow:: cachix
