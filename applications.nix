{ config, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      clementine
      gitg
      google-chrome
      qpdfview
      spotify
      terminator
    ];
  };
}
