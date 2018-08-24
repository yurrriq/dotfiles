{ config, lib, pkgs, ... }:

{
  imports = [
    ./applications.nix
    ./c.nix
    ./clojure.nix
    ./doc-prep.nix
    ./emacs.nix
    ./erlang.nix
    ./git.nix
    ./haskell.nix
    # ./idris.nix
    # FIXME: ./io.nix
    # FIXME: ./lilypond.nix
    ./lisp.nix
    ./maths.nix
    # ./ml.nix
    ./node.nix
    # ./scheme.nix
    ./shell.nix
    ./system-defaults.nix
  ];

  services = {
    activate-system.enable = true;
    nix-daemon = {
      enable = true;
      tempDir = "/nix/tmp";
    };
  };

  nix = {
    buildCores = 4;

    buildMachines = [
      # {
      #   hostName = "build-slave";
      #   system = "x86_64-linux";
      #   maxJobs = 2;
      # }
      {
        hostName = "nix-docker";
        system = "x86_64-linux";
        maxJobs = 2;
        sshKey = "/Users/mohacker/.ssh/docker_rsa";
      }
    ];

    distributedBuilds = true;

    gc.automatic = true;

    maxJobs = 8;

    nixPath = [
      "darwin=$HOME/.nix-defexpr/channels/darwin"
      "darwin-config=$HOME/.nixpkgs/darwin-configuration.nix"
      "nixpkgs=$HOME/.nix-defexpr/channels/nixpkgs"
      # "/nix/var/nix/profiles/per-user/root/channels"
    ];
  };

  nixpkgs.config.allowUnfree = true;
}
