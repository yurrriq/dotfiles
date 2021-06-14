{ lib, pkgs, ... }:
{
  imports = [
    ../../config/bash.nix
    ../../config/bat.nix
    ../../config/browserpass.nix
    ../../config/direnv.nix
    ../../config/dunst.nix
    ../../config/emacs
    ../../config/firefox.nix
    ../../config/fish
    ../../config/fzf.nix
    ../../config/git
    ../../config/gpg.nix
    ../../config/htop.nix
    ../../config/jq.nix
    ../../config/kitty.nix
    ../../config/man.nix
    ../../config/password-store.nix
    ../../config/rebar3.nix
    ../../config/starship.nix
    ../../config/taskwarrior
    ../../config/xmonad
  ];
  accounts.email.accounts.primary = {
    address = "eric@ericb.me";
    gpg.key = "F88372B24A806FF23BCB3A4E2DDDF8606958B3F9";
    primary = true;
    realName = "Eric Bailey";
  };
  home.packages = with pkgs; [
    appimage-run
    calibre
    fd
    frescobaldi
    lutris
    musescore
    openscad
    powertop
    reaper
    steam
    tellico
    zoom-us
  ];
}
