{ config, pkgs, ... }:

{

  home.file.".xmonad/skyrim.raw".source = ./xmonad/skyrim.raw;

  home.packages = with pkgs; [
    # FIXME
    # i3lock
  ];

  services.screen-locker = {
    enable = true;
    inactiveInterval = 5;
    # FIXME: ${pkgs.i3lock}/bin/i3lock
    lockCmd = ''
      i3lock \
        --image ${config.home.homeDirectory}/.xmonad/skyrim.raw \
        --raw 3840x2400:rgb
    '';
  };

}
