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
      qpdfview
      (signal-desktop.override { spellcheckerLanguage = "en_US"; })
    ]
  );
}
