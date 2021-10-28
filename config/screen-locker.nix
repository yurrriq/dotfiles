{ config, pkgs, ... }:

{

  services.screen-locker = {
    enable = true;
    inactiveInterval = 5;
    lockCmd = "${pkgs.i3lock}/bin/i3lock -i ${config.home.homeDirectory}/.xmonad/matrix.png";
  };

}
