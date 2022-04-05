{ config, pkgs, ... }:

{

  imports = [
    ../dunst.nix
    ../fonts.nix
    ../rofi.nix
  ];

  home.packages = with pkgs; [
    flameshot
    # font-awesome_4
    haskellPackages.xmobar
    playerctl
    xorg.xbacklight
  ];

  xdg.configFile."xmobar/xmobarrc" = {
    source = ../xmobar/xmobarrc;
    # NOTE: https://github.com/nix-community/home-manager/issues/1399
    onChange = ''
      if [[ -v DISPLAY ]]; then
          echo "Restarting xmonad"
          $DRY_RUN_CMD ${config.xsession.windowManager.command} --restart
      fi
    '';
  };

  home.pointerCursor = {
    package = pkgs.vanilla-dmz;
    name = "Vanilla-DMZ-AA";
    size = 36;
    x11.enable = true;
  };

  xsession = {
    enable = true;
    scriptPath = ".hm-xsession";
    windowManager.xmonad = {
      enable = true;
      extraPackages = hpkgs: [ hpkgs.xmonad-contrib ];
      config = ./xmonad.hs;
    };
  };

}
