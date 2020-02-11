{ config, lib, pkgs, ... }:

{

  home.packages = with pkgs; [
    albert
    flameshot
    i3lock
    rofi
    rofi-pass
  ];

  services.compton.enable = true;

  xdg.configFile."i3status/config".source = ../i3status/config;

  xdg.dataFile."i3/matrix.png".source = ./matrix.png;

  xsession.enable = true;

  xsession.windowManager.i3 = {
    enable = true;
    config = let modifier = "Mod4"; in {
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
        "${modifier}+space" = "exec --no-startup-id albert toggle";
        "Print" = "exec flameshot gui";
      };
      inherit modifier;
    };
  };

}
