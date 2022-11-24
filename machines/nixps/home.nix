{ pkgs, ... }:
{
  imports = [
    ../../config/applications.nix
    ../../config/bash.nix
    ../../config/bat.nix
    ../../config/browserpass.nix
    ../../config/clis.nix
    ../../config/direnv.nix
    ../../config/dunst.nix
    ../../config/emacs
    ../../config/firefox.nix
    ../../config/fish
    ../../config/fonts.nix
    ../../config/fzf.nix
    ../../config/git
    ../../config/gpg.nix
    ../../config/htop.nix
    ../../config/jq.nix
    ../../config/keyboard.nix
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
    amdvlk
    appimage-run
    calibre
    fd
    frescobaldi
    gnutls
    lutris
    musescore
    openscad
    powertop
    protontricks
    reaper
    # FIXME
    # (signal-desktop.override { spellcheckerLanguage = "en_US"; })
    steam
    tellico
    winetricks
    zoom-us
  ];
  home.stateVersion = "22.11";
  services.picom.enable = true;
}
