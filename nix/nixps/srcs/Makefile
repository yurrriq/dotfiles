.PHONY: $(wildcard *.json)


all: $(wildcard *.json)


%.json: branch=$(shell <$@ jq -r .branch)
%.json: owner=$(shell <$@ jq -r .owner)
%.json: repo=$(shell <$@ jq -r .repo)
%.json: rev=$(shell http "https://api.github.com/repos/${owner}/${repo}/git/refs/heads/${branch}" Accept:application/vnd.github.v3+json | jq -r '.object.sha')
%.json: sha256=$(shell nix-prefetch-url --unpack "https://github.com/${owner}/${repo}/tarball/${rev}")
*.json:
	<"$@" jq '.rev = "${rev}" | .sha256 = "${sha256}"' | sponge "$@"
