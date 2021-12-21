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
      # TODO(e.bailey): This takes a while to build and I rarely use it.
      # libreoffice
      qpdfview
      (signal-desktop.override { spellcheckerLanguage = "en_US"; })
    ]
  );
}
