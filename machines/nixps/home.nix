{ config, pkgs, ... }:
{
  accounts.email.accounts = {
    personal = {
      address = "eric@ericb.me";
      gpg.key = "F88372B24A806FF23BCB3A4E2DDDF8606958B3F9";
      primary = true;
      realName = "Eric Bailey";
    };
    work.address = "e.bailey@sportradar.com";
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
    signal-desktop
    steam
    tellico
    winetricks
    zoom-us
  ];
  home.stateVersion = "23.05";
  programs.rbw = {
    enable = true;
    settings = {
      email = config.accounts.email.accounts.personal.address;
    };
  };
  services.picom.enable = true;
}
