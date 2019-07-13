keyfile ?= ~/keybase/private/yurrriq/sruxps.tomb.key

.ONESHELL:
switch:
	@ tomb open -g secrets.tomb -k ${keyfile} secrets
	@ sudo nixos-rebuild switch
	@ tomb close secrets
