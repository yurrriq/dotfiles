{ config, pkgs, ... }:

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
    dhall
    dhall-json
    ghc
    gzip
    helmfile
    jdiskreport
    jdk
    kops
    kube-prompt
    kubectx
    kubernetes
    kubernetes-helm
    kubetail
    minikube
    nix
    nix-prefetch-git
    pandoc
    yq
    vim
    ] ++ (with haskellPackages; [
      # FIXME: hadolint
      # hindent
      # hpack
      # FIXME: hpack-convert
      stylish-haskell
    ]));

  environment.pathsToLink = [
    "/lib/aspell"
  ];

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      hack-font
      hasklig
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
    ]) ++ [
      (self: super: { nur = import <nur> { pkgs = super; }; })
    ];

  services.activate-system.enable = true;

  services.nix-daemon = {
    enable = true;
    tempDir = "/nix/tmp";
  };

}
