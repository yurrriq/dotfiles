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
    font = "Iosevka Custom 18";
    pass = {
      enable = true;
      extraConfig = ''
        help_color="#003152"
      '';
      stores = [ "~/.password-store" ];
    };
    plugins = with pkgs; [
      rofi-calc
      rofi-top
    ];
    theme = "purple";
  };
}
