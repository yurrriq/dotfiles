{ config, pkgs, ... }:

{

  home.file.".xmonad/matrix.png".source = ./matrix.png;

  home.packages = with pkgs; [
    i3lock
  ];

  services.screen-locker = {
    enable = true;
    inactiveInterval = 5;
    lockCmd = "${pkgs.i3lock}/bin/i3lock -i ${config.home.homeDirectory}/.xmonad/matrix.png";
  };

}
