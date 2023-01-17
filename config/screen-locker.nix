{ config, pkgs, ... }:

{

  home.file.".xmonad/matrix.png".source = ./xmonad/matrix.png;

  home.packages = with pkgs; [
    # FIXME
    # i3lock
  ];

  services.screen-locker = {
    enable = true;
    inactiveInterval = 5;
    # FIXME: ${pkgs.i3lock}/bin/i3lock
    lockCmd = "i3lock -i ${config.home.homeDirectory}/.xmonad/matrix.png";
  };

}
