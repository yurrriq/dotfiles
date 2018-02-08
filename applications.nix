{ config, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      google-chrome
      qpdfview
      terminator
    ];
  };
}
