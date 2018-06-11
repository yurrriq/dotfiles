{ config, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      # TODO: clementine
      google-chrome
      libreoffice
      qpdfview
      spotify
      terminator
      thunderbird
    ];
  };
}
