{ pkgs, ... }:

{

  home.file.".xmonad/matrix.png".source = ./matrix.png;

  home.packages = with pkgs; [
    flameshot
    # font-awesome_4
    i3lock
    rofi-pass # https://github.com/nix-community/home-manager/pull/1427
    haskellPackages.xmobar
  ];

  programs.rofi = {
    enable = true;
    font = "Iosevka Term 18";
    theme = "purple";
  };

  services.network-manager-applet.enable = true;

  services.pasystray.enable = true;

  services.picom.enable = true;

  services.stalonetray = {
    enable = false;
    config = {
      background = "#001e26";
      decorations = "none";
      dockapp_mode = "none";
      geometry = "5x1-860-0";
      grow_gravity = "NE";
      icon_gravity = "NE";
      icon_size = 43;
      kludges = "force_icons_size";
      no_shrink = false;
      skip_taskbar = true;
      sticky = true;
      transparent = false;
      window_layer = "bottom";
      window_strut = "none";
      window_type = "dock";
    };
  };

  xdg.configFile."xmobar/xmobarrc".source = ../xmobar/xmobarrc;

  xsession = {
    enable = true;
    pointerCursor = {
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ-AA";
      size = 36;
    };
    scriptPath = ".hm-xsession";
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./xmonad.hs;
    };
  };

}
