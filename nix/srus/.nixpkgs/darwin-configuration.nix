{ config, lib, pkgs, ... }:

let
  _nixpkgs =
    let
      inherit (builtins) fetchTarball fromJSON readFile;
      inherit (fromJSON (readFile ./nixpkgs-src.json)) owner repo rev sha256;
    in
    fetchTarball {
      url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
      inherit sha256;
    };
    # "$HOME/src/github.com/NixOS/nixpkgs";
    # "$HOME/.nix-defexpr/channels/nixpkgs";
in

{
  imports = [
    ./modules/beam.nix
    ./modules/dhall.nix
    ./modules/gap.nix
    ./modules/git.nix
    ./modules/haskell.nix
    ./modules/node.nix
    ./modules/k8s.nix
    ./modules/shell.nix
    ./modules/system-defaults.nix
  ];

  environment.systemPackages = (with pkgs; [
    aspell
    aspellDicts.en
    clementine
    # copyq
    emacs
    gcc
    gnupg
    graphviz
    jdk
    # FIXME: kdiff3
    ncurses
    nix
    nix-prefetch-git
    pandoc
    pcre
    python
    python3
    skim
    vim
    zlib
    ]) ++ (with pkgs.python27Packages; [
      pywatchman
    ]) ++ (with pkgs.python35Packages; [
      pygments
    ]);

  # Recreate /run/current-system symlink after boot.
  services.activate-system.enable = true;

  services.nix-daemon = {
    enable = true;
    tempDir = "/nix/tmp";
  };

  environment.pathsToLink =
    [ # "/bin"
      "/lib/aspell"
      # "/share/info"
      # "/share/locale"
      "/share/emacs"
      # "/Applications"
    ];

  nix = {
    buildCores = 8;
    # TODO: buildMachines = [];
    distributedBuilds = false;
    gc.automatic = true;
    maxJobs = 8;
    nixPath = [
      "darwin=$HOME/.nix-defexpr/channels/darwin"
      "darwin-config=$HOME/.nixpkgs/darwin-configuration.nix"
      "nixpkgs=${_nixpkgs}"
    ];
  };

  nixpkgs.config.allowUnfree = true;

  nixpkgs.config.packageOverrides = super: {
    clementine = super.callPackage ./pkgs/applications/audio/clementine {};
    copyq = super.callPackage ./pkgs/applications/misc/copyq {};
    jdk = super.openjdk8;
    skim = super.callPackage ./pkgs/applications/misc/skim {};
  };
}
