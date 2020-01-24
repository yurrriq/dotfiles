{ config, lib, ... }:

let

  sources = import ./sources.nix;

  inherit (sources) darwin home-manager nur;

  seemsDarwin = null != builtins.match ".*darwin$" builtins.currentSystem;

  nixpkgs = if seemsDarwin then sources.nixpkgs-darwin else sources.nixpkgs;

  inherit (config) nurpkgs;
in

{

  options.nurpkgs = lib.mkOption {
    default = ./nurpkgs.nix;
    type = lib.types.path;
  };

  config = {
    nix.nixPath = [
      "home-manager=${home-manager}"
      "nixpkgs=${nixpkgs}"
      "nur=${nur}"
      "nurpkgs=${nurpkgs}"
    ] ++ lib.optional seemsDarwin "darwin=${darwin}";
  };

}
