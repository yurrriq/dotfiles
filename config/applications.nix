{ lib, pkgs, ... }:
{
  home.packages = with pkgs; (
    [
      clementine
      pavucontrol
      slack
    ]
  ) ++ lib.optionals stdenv.isLinux (
    [
      qpdfview
    ]
  );
}
