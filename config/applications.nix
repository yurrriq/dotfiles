{ lib, pkgs, ... }:
{
  home.packages = with pkgs; (
    [
      clementine
      pavucontrol
      slack
      spotify
    ]
  ) ++ lib.optionals stdenv.isLinux (
    [
      qpdfview
    ]
  );
}
