{ config, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      clementine
      gitg
      google-chrome
      nix
      qpdfview
      spotify
      terminator
      # TODO: thunderbird
    ];
  };
}
