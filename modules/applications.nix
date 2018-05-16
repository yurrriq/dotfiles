{ config, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      # TODO: clementine
      google-chrome
      qpdfview
      # FIXME: spotify
      terminator
      # TODO: thunderbird
    ];
  };
}
