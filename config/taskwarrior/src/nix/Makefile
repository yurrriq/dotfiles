owner ?= NixOS
repo ?= nixpkgs
rev ?= master

nixpkgs.json:
	@ nix-prefetch-github ${owner} ${repo} --rev ${rev} >$@
