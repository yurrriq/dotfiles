{ lib, pkgs, ... }:
let
  nixpkgs-unstable = import (import ../../nix/sources.nix).nixpkgs-unstable {
    config.allowUnfree = true;
  };
in
{

  imports = [
    ../../config/bash.nix
    ../../config/bat.nix
    ../../config/browserpass.nix
    ../../config/bugwarrior.nix
    ../../config/direnv.nix
    ../../config/dunst
    ../../config/emacs
    ../../config/firefox.nix
    ../../config/fish
    ../../config/fzf.nix
    ../../config/git
    ../../config/gpg.nix
    ../../config/htop.nix
    ../../config/i3
    ../../config/jq.nix
    ../../config/kitty.nix
    ../../config/man.nix
    ../../config/nixpkgs
    ../../config/rebar3.nix
    ../../config/taskwarrior
  ];

  accounts.email.accounts.primary = {
    address = "e.bailey@sportradar.com";
    gpg.key = "EFD6F1EDC84D2FA935E38570462054AB8B682702";
    primary = true;
    realName = "Eric Bailey";
  };

  home.packages = with pkgs; (
    [
      aws-iam-authenticator
      ec2instanceconnectcli
      clac
      dolphin
      lorri
      mtr
      progress
      pv
      nixpkgs-unstable.clementineUnfree
      docker-compose
      ghc
      iw
      jdk
      nixpkgs-unstable.kubectx
      kubelogin
      k9s
      naal
      networkmanager-openconnect
      openconnect
      pavucontrol
      renderizer
      scc
      lm_sensors
      nfs-utils
      libnotify
      dunst
      paper-icon-theme
      powertop
      wirelesstools
      nixpkgs-unstable.zoom-us
    ] ++ (
      with nodePackages; [
        aws-azure-login
      ]
    )
  );

  _module.args.pkgs = lib.mkForce pkgs;

}
