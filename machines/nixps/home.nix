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
  xdg.configFile."REAPER" = {
    source = pkgs.symlinkJoin {
      name = "reaper-userplugins";
      paths = with pkgs; [
        # reaper-sws-extension
        reaper-reapack-extension
      ];
    };
    recursive = true;
  };
  home.packages = with pkgs; [
    calibre
    devenv
    duf
    fd
    gnutls
    lutris
    openscad
    powertop
    protontricks
    signal-desktop
    steam
    tellico
    winetricks
    # zoom-us
    alsa-scarlett-gui
    alsa-utils
    # frescobaldi
    # (
    #   musescore.overrideAttrs (old: {
    #     qtWrapperArgs = old.qtWrapperArgs ++ [
    #       "--set QT_SCREEN_SCALE_FACTORS 2"
    #     ];
    #   })
    # )
    (
      reaper.overrideAttrs (oldAttrs: {
        nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ [ makeWrapper ];
        postInstall = (oldAttrs.postInstall or "") + ''
          wrapProgram $out/bin/reaper \
            --prefix LD_LIBRARY_PATH : "${lib.makeLibraryPath [
              glibc
              stdenv.cc.cc
              udev
              xdotool
              xorg.libX11
            ]}"
        '';
      })
    )
    scarlett2
  ];
  home.stateVersion = "25.11";
  programs.rbw = {
    enable = true;
    settings = {
      base_url = "https://api.bitwarden.com/";
      email = config.accounts.email.accounts.personal.address;
      identity_url = "https://identity.bitwarden.com/";
      notifications_url = "https://notifications.bitwarden.com/";
      pinentry = pkgs.pinentry-gnome3;
    };
  };
  services.picom.enable = true;
}
