{ config, lib, ... }:

with (import ./sources.nix);
let
  inherit (config) nurpkgs;
  inherit ((import nurpkgs { }).lib) seemsDarwin;
in
{

  options.nurpkgs = lib.mkOption {
    default = ./nurpkgs.nix;
    type = lib.types.path;
  };

  config = {
    nix.nixPath = [
      "home-manager=${home-manager}"
      "nixos-hardware=${nixos-hardware}"
      "nixpkgs=${if seemsDarwin then nixpkgs-darwin else nixpkgs}"
      "nixpkgs-unstable=${nixpkgs-unstable}"
      "nur=${nur}"
      "nurpkgs=${nurpkgs}"
    ] ++ lib.optional seemsDarwin "darwin=${darwin}";
  };

}
