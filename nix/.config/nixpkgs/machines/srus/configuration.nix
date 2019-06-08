{ config, lib, pkgs, ... }:

with import <setup/srcs> { local = false; };

let

  inherit (nur-no-pkgs.repos.yurrriq.lib) pinnedNixpkgs;

  username = "e.bailey";

in

{
  imports = [
    <setup/common.nix>
    <setup/darwin.nix>
    <setup/packages.nix>
  ];

  environment = {
    darwinConfig = "$HOME/.config/nixpkgs/machines/srus/configuration.nix";
    pathsToLink = [
      "/lib/aspell"
      "/share/emacs/site-lisp"
    ];
    systemPackages = with pkgs; ([
      cabal2nix
      ghc
      jdk
      sops
    ] ++ (with haskellPackages; [
      # FIXME: hadolint
      # hindent
      # hpack
      # FIXME: hpack-convert
      stylish-haskell
    ]) ++ (with nodePackages; [
      aws-azure-login
      nodePackages."mermaid.cli"
      vmd
    ]));
  };

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      iosevka
    ];
   };

  nix = {

    buildCores = 8;

    # TODO: buildMachines = [];

    distributedBuilds = false;

    gc = {
      # user = username;
    };

    maxJobs = 8;

    nixPath = lib.mkForce [
      "darwin=${_darwin}"
      "darwin-config=$HOME/.config/nixpkgs/machines/srus/configuration.nix"
      "nixpkgs=${_nixpkgs}"
      "nixpkgs-overlays=$HOME/.config/nixpkgs/overlays"
      "nur=${_nur}"
      "setup=$HOME/.config/nixpkgs/setup"
    ];

    trustedUsers = [ "root" username ];

  };

  nixpkgs.config.allowUnfree = true;

  nixpkgs.overlays =
    let path = <nixpkgs-overlays>; in with builtins;
      map (n: import (path + ("/" + n)))
          (filter (n: match ".*\\.nix" n != null ||
                      pathExists (path + ("/" + n + "/default.nix")))
                  (attrNames (readDir path))) ++
    (with nur-no-pkgs.repos.yurrriq.overlays; [
      nur
      git
      node
    ]);

  services = {
    activate-system.enable = true;
    nix-daemon = {
      enable = true;
      tempDir = "/nix/tmp";
    };
  };

}
