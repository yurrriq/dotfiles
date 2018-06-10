{ config, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      # TODO: clementine
      google-chrome
      libreoffice
      qpdfview
      # FIXME: spotify
      terminator
      thunderbird
    ];
  };
}
