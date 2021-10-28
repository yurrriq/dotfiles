{ lib, pkgs, ... }:
{
  environment.systemPackages = with pkgs; (
    [
      clementine
      pavucontrol
      slack
      spotify
    ]
  ) ++ lib.optionals stdenv.isLinux (
    [
      dolphin
      libreoffice
      qpdfview
      # FIXME: Electron 11.5.0 EOL
      # (signal-desktop.override { spellcheckerLanguage = "en_US"; })
    ]
  );
}
