{ config, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      clementine
      gitg
      google-chrome
      nix
      noweb
      qpdfview
      spotify
      terminator
      # TODO: thunderbird
    ];
  };
}
