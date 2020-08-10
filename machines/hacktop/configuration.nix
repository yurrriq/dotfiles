{ config, lib, pkgs, ... }:
let
  username = "mohacker";
in
{

  imports = [
    ../../nix
    ../../modules/common.nix
    ../../modules/darwin.nix
    ../../modules/location.nix
    ../../modules/packages.nix
    <home-manager/nix-darwin>
  ];

  airportCode = "MSP";

  environment.darwinConfig = "$HOME/.config/nixpkgs/machines/hacktop/configuration.nix";

  environment.pathsToLink = [
    "/lib/aspell"
    "/share/emacs/site-lisp"
    "/share/fish"
  ];

  home-manager.useUserPackages = true;
  home-manager.users."${username}" = args:
    import ./home.nix (args // { inherit pkgs; });

  nix = {
    buildCores = 8;

    distributedBuilds = false;

    maxJobs = 8;

    nixPath = lib.mkForce [
      "darwin=${darwin}"
      "darwin-config=/etc/nixos/configuration.nix"
      "home-manager=${home-manager}"
      "nixpkgs=${nixpkgs}"
      "nixpkgs-overlays=/etc/nixos/overlays"
      "nur=${nur}"
      "nurpkgs=${nurpkgs}"
    ];

    trustedUsers = [ "root" username ];
  };

  nixpkgs.overlays =
    let
      path = <nixpkgs-overlays>;
    in
    with builtins;
    map
      (n: import (path + ("/" + n)))
      (
        filter
          (
            n: match ".*\\.nix" n != null
            || pathExists (path + ("/" + n + "/default.nix"))
          )
          (attrNames (readDir path))
      )
    ++ (
      with (import <nurpkgs> { }).overlays; [
        nur
        git
        node
      ]
    );

  services = {
    activate-system.enable = true;
    nix-daemon = {
      enable = true;
      tempDir = "/nix/tmp";
    };
  };

}
