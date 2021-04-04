{ lib, pkgs, ... }:
{
  environment.systemPackages = with pkgs; (
    [
      clementine
      pavucontrol
      spotify
      slack
    ]
  ) ++ lib.optionals stdenv.isLinux (
    [
      dolphin
      libreoffice
      qpdfview
      (signal-desktop.override { spellcheckerLanguage = "en_US"; })
    ]
  ) ++ lib.optionals stdenv.isDarwin (
    [
      m-cli
      onyx
      sourcetree
    ]
  );
}
