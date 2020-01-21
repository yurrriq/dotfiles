{ config, lib, pkgs, ... }:

with import ../../modules/nix { local = false; };

let

  username = "mohacker";

  airportCode = "MSP";

in

{
  imports = [
    ../../modules/common.nix
    ../../modules/darwin.nix
    (import ../../modules/location.nix { inherit lib airportCode; })
    ../../modules/packages.nix
    "${home-manager}/nix-darwin"
  ];

  environment = {
    darwinConfig = "$HOME/.config/nixpkgs/machines/hacktop/configuration.nix";
    pathsToLink = [
      "/lib/aspell"
      "/share/emacs/site-lisp"
      "/share/fish"
    ];
    systemPackages = with pkgs; ([
      cabal2nix
      ghc
    ] ++ (with haskellPackages; [
      # FIXME: hadolint
      # hindent
      # hpack
      # FIXME: hpack-convert
      stylish-haskell
    ]) ++ (with nodePackages; [
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

  home-manager.useUserPackages = true;
  home-manager.users."${username}" = args:
    import ./home.nix (args // { inherit pkgs; });


  nix = {

    buildCores = 8;

    # TODO: buildMachines = [];

    distributedBuilds = false;

    gc = {
      # user = username;
    };

    maxJobs = 8;

    nixPath = lib.mkForce [
      "darwin=${darwin}"
      "darwin-config=/etc/nixos/configuration.nix"
      "nixpkgs=${nixpkgs}"
      "nixpkgs-overlays=/etc/nixos/overlays"
      "nur=${nur}"
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
      # engraving
      git
      # hadolint
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
