keyfile ?= ~/keybase/private/yurrriq/sruxps.tomb.key

.ONESHELL:

dry-build: open-secrets _dry-build close-secrets

switch:	open-secrets _switch close-secrets

_dry-build:
	@ nixos-rebuild dry-build

_switch:
	@ sudo nixos-rebuild switch

open-secrets:
	@ tomb open -g secrets.tomb -k ${keyfile} secrets

close-secrets:
	@ tomb close secrets

slam-secrets:
	@ tomb slam secrets
