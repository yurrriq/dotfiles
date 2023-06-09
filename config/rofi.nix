{ config, lib, pkgs, ... }:

{
  imports = [
    ./fonts.nix
  ];

  home.packages = with pkgs; lib.optionals config.programs.rbw.enable [
    rofi-rbw
    xdotool
  ];

  programs.rofi = {
    enable = true;
    font = "Iosevka Nerd Font 18";
    pass = {
      enable = true;
      stores = [ "~/.password-store" ];
    };
    theme = "purple";
  };
}
