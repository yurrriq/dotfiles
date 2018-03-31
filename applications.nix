{ config, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      clementine
      gitg
      google-chrome
      noweb
      qpdfview
      spotify
      terminator
      # TODO: thunderbird
    ];
  };
}
