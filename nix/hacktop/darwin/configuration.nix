{ config, lib, pkgs, ... }:

with import ../srcs { local = false; };

{
  imports = [
    <setup/common.nix>
    <setup/darwin.nix>
    <setup/packages.nix>
    # ../config/clojure.nix
    # ../config/doc-prep.nix
    ../config/emacs.nix
    # ../config/engraving.nix
    ../config/haskell.nix
    # ../config/lisp.nix
    # ../config/ml.nix
    # ../config/scheme.nix
    # TODO: ../config/theorem-proving.nix
  ];

  services = {
    activate-system.enable = true;
    nix-daemon = {
      enable = true;
      tempDir = "/nix/tmp";
    };
  };

  nix = {

    buildCores = 8;

    buildMachines = [];

    distributedBuilds = false;

    gc.automatic = true;

    maxJobs = 8;

    nixPath = [
      "darwin=${_darwin}"
      "darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix"
      "nixpkgs=${_nixpkgs}"
      # FIXME: "nixpkgs-overlays=$HOME/config/.nixpkgs/overlays"
      "setup=$HOME/.config/nixpkgs/setup"
    ];

    trustedUsers = [ "root" "mohacker" ];
  };

  nixpkgs.config.allowUnfree = true;

  # TODO: https://github.com/peel/dotfiles/blob/1e00dacf/nix/.config/nixpkgs/darwin/configuration.nix#L12-L18
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
        erlang
        m-cli
        musescore
        onyx
        skim
        spotify;
    })
    (self: super: { inherit (super.nur.repos.peel) chunkwm skhd; })
  ];
}
