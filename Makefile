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


NIX_SRCS := $(addsuffix .nix,\
$(addprefix config/,\
bash \
bat \
browserpass \
bugwarrior \
direnv \
dunst/default \
emacs/default \
emacs/packages \
firefox \
fish/abbrs \
fish/aliases \
fish/default \
fzf \
git/aliases \
git/config \
git/default \
git/packages \
gpg \
htop \
i3/default \
jq \
kitty \
man \
nixpkgs/default \
nixpkgs/nixpkgs-config \
password-store \
rebar3 \
starship \
taskwarrior/default \
xmonad/default \
)\
$(addprefix machines/,\
hacktop/configuration \
hacktop/home \
nixps/configuration \
nixps/home \
sruxps/configuration \
sruxps/home \
)\
$(addprefix modules/,\
common \
darwin \
location \
nixos \
packages \
))

SH_SRCS := \
config/taskwarrior/on-exit-git.sh

OTHER_SRCS := \
config/dunst/dunstrc \
config/emacs/init.el \
config/fish/interactiveShellInit.fish \
config/fish/shellInit.fish \
config/fish/sushi/fish_prompt.fish \
config/fish/sushi/fish_right_prompt.fish
# config/i3status/config

# NW_SRCS := \
# $(patsubst %.nix,src/%.nw,${NIX_SRCS}) \
# $(patsubst %.sh,src/%.nw,${SH_SRCS})
NW_SRCS := $(shell find src -name '*.nw')

TEX_SRCS := $(patsubst src/%.nw,src/%.tex,${NW_SRCS})

DEFS := $(patsubst src/%.nw,src/%.defs,${NW_SRCS})

.PHONY: all
all: generate-config nix-srcs ${SH_SRCS} ${OTHER_SRCS} docs/dotfiles.pdf


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
	@ latexmk $(latexmk_flags) -c -f docs/dotfiles.pdf
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
	@ latexmk $(latexmk_flags) -outdir=../$(@D) $<
	@ noindex src/dotfiles
	@ latexmk $(latexmk_flags) -outdir=../$(@D) $<


src/%.defs: src/%.nw
	nodefs $< >$@


src/all.defs: ${DEFS}
	sort -u $^ ${cpif} $@


# .INTERMEDIATE: ${TEX_SRCS}
src/%.tex: src/%.nw src/all.defs
	noweave -delay -indexfrom src/all.defs -latex -n -filter fix-underscores $^ ${cpif} $@


${NIX_SRCS} ${OTHER_SRCS} ${SH_SRCS}::
	@ mkdir -p $(@D)
	notangle -R$@ src/$(basename $@).nw ${cpif} $@


${SH_SRCS}::
	chmod a+x $@


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


bootstrap: command=nixos-rebuild build
bootstrap:
	@ sudo ${command} \
	-I home-manager=$$(jq -r '."home-manager".url' nix/sources.json) \
	-I niv=$$(jq -r '.niv.url' nix/sources.json) \
	-I nixos-hardware=$$(jq -r '."nixos-hardware".url' nix/sources.json) \
	-I nixpkgs=$$(jq -r '.nixpkgs.url' nix/sources.json) \
	-I nixpkgs-overlays=/etc/nixos/overlays \
	-I nixpkgs-unstable=$$(jq -r '."nixpkgs-unstable".url' nix/sources.json) \
	-I nur=$$(jq -r '.nur.url' nix/sources.json) \
	-I nurpkgs=/etc/nixos/nix/nurpkgs.nix \
	--show-trace


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
	@ jq '"${package}: ${rev} -> \(.["${package}"].rev[:8])"' \
	${sources} | xargs git commit -m


.PHONY: generate-config
generate-config: machines/${machine}/hardware-configuration.nix

machines/${machine}/hardware-configuration.nix:
	nixos-generate-config --root ${PWD} --dir /$(@D)
	nixpkgs-fmt $@
