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
    calibre
    devenv
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
    # zoom-us
  ];
  home.stateVersion = "25.05";
  programs.rbw = {
    enable = true;
    settings = {
      base_url = "https://api.bitwarden.com/";
      email = config.accounts.email.accounts.personal.address;
      identity_url = "https://identity.bitwarden.com/";
      notifications_url = "https://notifications.bitwarden.com/";
      pinentry = pkgs.pinentry;
    };
  };
  services.picom.enable = true;
}
