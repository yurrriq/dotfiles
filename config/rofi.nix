{ config, lib, pkgs, ... }:

{
  imports = [
    ./fonts.nix
  ];

  home.packages = with pkgs; [
    rofi-bluetooth
    rofi-systemd
  ] ++ lib.optionals config.programs.rbw.enable [
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
    plugins = with pkgs; [
      rofi-calc
      rofi-top
    ];
    theme = "purple";
  };
}
