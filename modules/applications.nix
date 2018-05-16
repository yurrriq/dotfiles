{ config, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      # TODO: clementine
      gitg
      google-chrome
      qpdfview
      # FIXME: spotify
      terminator
      # TODO: thunderbird
    ];
  };
}
