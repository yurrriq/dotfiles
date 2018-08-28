{ config, lib, pkgs, ... }:

with import ./srcs { local = false; };

{
  imports = [
    ./config/applications.nix
    ./config/c.nix
    # ./config/clojure.nix
    # ./config/doc-prep.nix
    ./config/emacs.nix
    # ./config/engraving.nix
    # TODO: ./config/erlang.nix
    ./config/git.nix
    ./config/haskell.nix
    # ./config/lisp.nix
    ./config/maths.nix
    # ./config/ml.nix
    ./config/node.nix
    # ./config/scheme.nix
    ./config/shell.nix
    ./config/system-defaults.nix
    # TODO: ./config/theorem-proving.nix
  ];

  environment.systemPackages = with pkgs; [
    diff-pdf
  ];

  services = {
    activate-system.enable = true;
    nix-daemon = {
      enable = true;
      tempDir = "/nix/tmp";
    };
  };

  nix = {

    binaryCaches = [
      # "https://cache.nixos.org"
      "https://yurrriq.cachix.org"
      "https://yurrriq-nur-packages.cachix.org"
    ];

    binaryCachePublicKeys = [
      # "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "yurrriq.cachix.org-1:evpJ5wKluf7QOCcv69VkIxCOtHgubrqXlZpp3JAXLBE="
      "yurrriq-nur-packages.cachix.org-1:7kbjuGBUZcWf876g2cdelmIQXrXzOhpMVBqYOyyAv70="
    ];

    buildCores = 8;

    buildMachines = [];

    distributedBuilds = false;

    gc.automatic = true;

    maxJobs = 8;

    nixPath = [
      "darwin=${_darwin}"
      "darwin-config=$HOME/.nixpkgs/darwin-configuration.nix"
      "nixpkgs=${_nixpkgs}"
      # FIXME: "nixpkgs-overlays=$HOME/.nixpkgs/overlays-compat/"
    ];

    trustedUsers = [ "root" "mohacker" ];
  };

  nixpkgs.config.allowUnfree = true;

  nixpkgs.overlays = (with nur-no-pkgs.repos.yurrriq.overlays; [
    nur
    engraving
    git
    hadolint
    node
  ]) ++ [
    (self: super: { nur = import _nur { pkgs = super; }; })
    (self: super: {
      inherit (super.nur.repos.yurrriq.pkgs)
        autojump
        clementine
        m-cli
        musescore
        onyx
        skim
        spotify;
    })
    (self: super: { inherit (super.nur.repos.peel) chunkwm skhd; })
  ];
}
