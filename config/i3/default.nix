{ config, lib, pkgs, ... }:

{

  xdg.configFile.i3status = {
    target = "i3status/config";
    text = ''
      general {
          output_format = "i3bar"
          colors = true
          interval = 5
      }

      order += "load"

      load {
          format = "%1min %5min %15min"
      }

      order += "battery all"

      battery all {
          status_chr = "⚡"
          status_bat = "↯"
          status_unk = "‽"
          status_full = ""
          format = "%status %percentage %remaining"
      }

      order += "tztime local"

      tztime local {
          format = "%a %d.%m %H:%M:%S"
      }
    '';
  };

  xdg.dataFile = {
    "i3/matrix.png".source = ./matrix.png;
  };

  xsession.enable = true;

  xsession.windowManager.i3 = {
    enable = true;
    config = let modifier = "Mod4"; in {
      fonts = ["Iosevka 12"];
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
        "${modifier}+Escape" = "exec i3lock -i ${config.xdg.dataHome}/i3/matrix.png";
        "${modifier}+Shift+minus" = "move scratchpad";
        "${modifier}+minus" = "scratchpad show";
      };
      inherit modifier;
    };
  };

}
