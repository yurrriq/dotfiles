owner  ?= NixOS
repo   ?= nixpkgs
branch ?= nixpkgs-18.03
rev    ?= $(shell http "https://api.github.com/repos/NixOS/nixpkgs-channels/git/refs/heads/${branch}" Accept:application/vnd.github.v3+json | jq -r '.[0].object.sha')


.PHONY: nixpkgs-src.json
nixpkgs-src.json:
ifeq (${rev},)
	$(error Usage: make $@ rev=<nixpkgs revision>)
endif
	@ nix-prefetch-url --unpack \
		"https://github.com/${owner}/${repo}/archive/${rev}.tar.gz" | \
		xargs -I % jq -n \
			--arg owner "${owner}" \
			--arg repo "${repo}" \
			--arg rev "${rev}" \
			--arg sha256 % \
			'{owner: $$owner, repo: $$repo, rev: $$rev, sha256: $$sha256}' \
			>"$@"


.PHONY: build
build:
	@ nixos-rebuild build -j8


.PHONY: switch
switch:
	@ sudo nixos-rebuild switch
