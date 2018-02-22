{ config, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      clementine
      gitg
      google-chrome
      # FIXME: noweb
      qpdfview
      spotify
      terminator
      # TODO: thunderbird
    ];
  };
}
