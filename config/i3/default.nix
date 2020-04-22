{ config, lib, pkgs, ... }:

{

  home.packages = with pkgs; [
    flameshot
    i3lock
    rofi
    rofi-pass
  ];

  services.picom.enable = true;

  xdg.configFile."i3status/config".source = ../i3status/config;

  xdg.dataFile."i3/matrix.png".source = ./matrix.png;

  xsession.enable = true;

  xsession.windowManager.i3 = {
    enable = true;
    config =
      let
        colors = rec {
          background = "#001e26"; # "#002b36";
          focused = {
            border = "#001e26"; # "#002b36";
            background = "#586e75";
            text = "#fdf6e3";
            indicator = "#268bd2";
            childBorder = ""; # "#285577";
          };
          focusedInactive = {
            border = "#001e26"; # "#002b36";
            background = "#073642";
            text = "#839496";
            indicator = "#073642";
            childBorder = ""; # "#5f676a";
          };
          unfocused = {
            border = "#001e26"; # "#002b36";
            background = "#073642";
            text = "#839496";
            indicator = "#073642";
            childBorder = ""; # "#222222";
          };
          urgent = {
            border = "#001e26"; # "#002b36";
            background = "#dc322f";
            text = "#fdf6e3";
            indicator = "#001e26"; # "#002b36";
            childBorder = ""; # "#900000";
          };
        };
        barColors = {
          inherit (colors) background;
          focusedWorkspace = {
            border = "#b58900";
            background = "#b58900";
            text = "#001e26"; # "#002b36";
          };
          inactiveWorkspace = {
            border = "#073642";
            background = "#001e26"; # "#002b36";
            text = "#839496";
          };
          separator = "#586e75";
          statusline = "#839496";
          urgentWorkspace = {
            border = "#dc322f";
            background = "#dc322f";
            text = "#fdf6e3";
          };
        };
        modifier = "Mod4";
      in
        {
          bars = [
            {
              fonts = [ "Iosevka Term" ];
              statusCommand = "${pkgs.i3status}/bin/i3status";
              colors = barColors;
            }
          ];
          inherit colors;
          fonts = [ "Iosevka 12" ];
          keybindings = lib.mkOptionDefault {
            "XF86MonBrightnessDown" = "exec xbacklight -dec 10";
            "XF86MonBrightnessUp" = "exec xbacklight -inc 10";
            "XF86AudioMute" = "exec --no-startup-id amixer set Master 1+ toggle";
            "XF86AudioLowerVolume" = "exec --no-startup-id amixer set Master 5%-";
            "XF86AudioRaiseVolume" = "exec --no-startup-id amixer set Master 5%+";
            "XF86AudioPrev" = "exec playerctl previous";
            # "XF86AudioPlayPause" = "exec playerctl play-pause";
            "XF86AudioPlay" = "exec playerctl play-pause";
            # "XF86AudioPause" = "exec playerctl play-pause";
            "XF86AudioNext" = "exec playerctl next";
            "${modifier}+Escape" = ''
              exec i3lock -i ${config.xdg.dataHome}/i3/matrix.png
            '';
            "${modifier}+Shift+minus" = "move scratchpad";
            "${modifier}+minus" = "scratchpad show";
            "${modifier}+Tab" = "exec --no-startup-id rofi -show window";
            "${modifier}+d" = ''
              exec --no-startup-id "rofi -combi-modi run,window,drun -show combi -modi combi"
            '';
            "${modifier}+space" = ''
              exec --no-startup-id "rofi -combi-modi run,window,drun -show combi -modi combi"
            '';
            "Print" = "exec flameshot gui";
          };
          inherit modifier;
          startup = [
            {
              command = "volumeicon";
              notification = false;
            }
          ];
        };
  };

}
