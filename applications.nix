{ config, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      clementine
      gcc
      gitg
      google-chrome
      # FIXME: noweb
      qpdfview
      spotify
      terminator
      texlive.combined.scheme-full
    ];
  };
}
