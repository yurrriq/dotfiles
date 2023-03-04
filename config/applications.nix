{ lib, pkgs, ... }:
{
  home.packages = with pkgs; (
    [
      clementine
      spotify
      pavucontrol
      slack
    ]
  ) ++ lib.optionals stdenv.isLinux (
    [
      qpdfview
    ]
  );
}
