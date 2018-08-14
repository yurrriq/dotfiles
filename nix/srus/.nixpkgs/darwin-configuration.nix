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
    ./config/applications.nix
    ./config/beam.nix
    ./config/dhall.nix
    ./config/gap.nix
    ./config/git.nix
    ./config/haskell.nix
    ./config/java.nix
    ./config/node.nix
    ./config/k8s.nix
    ./config/shell.nix
    ./config/system-defaults.nix
  ];

  environment.systemPackages = (with pkgs; [
    aspell
    aspellDicts.en
    emacs
    gcc
    gnupg
    graphviz
    ncurses
    nix
    nix-prefetch-git
    pandoc
    pcre
    python
    python3
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
}
