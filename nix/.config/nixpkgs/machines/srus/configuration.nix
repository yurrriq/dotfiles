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

  environment.systemPackages = with pkgs; ([
    aspell
    aspellDicts.en
    cabal2nix
    ghc
    gzip
    jdk
    nix
    sops
    yq
    vim
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

  environment.pathsToLink = [
    "/lib/aspell"
    "/share/emacs/site-lisp"
  ];

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
      automatic = true;
      options = "--delete-older-than 30d";
      # user = username;
    };

    maxJobs = 8;

    nixPath = [
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

  services.activate-system.enable = true;

  services.nix-daemon = {
    enable = true;
    tempDir = "/nix/tmp";
  };

}
