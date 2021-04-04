{ pkgs, ... }:

{
  services.chunkwm = {
    enable = true;
    extraConfig = ''
      chunkc core::log_file /usr/local/var/log/chunkwm.log
      chunkc core::log_level info
      chunkc tiling::rule --owner "Cisco AnyConnect Secure Mobility Client" --state float &
      chunkc tiling::rule --owner Emacs --except "^$" --state tile &
    '';
    package = pkgs.chunkwm.core;
    plugins = {
      dir = "/run/current-system/sw/bin/chunkwm-plugins/";
      list = [ "ffm" "tiling" ];
      "tiling".config = ''
        chunkc set desktop_padding_step_size     0
        chunkc set desktop_gap_step_size         0
        chunkc set global_desktop_mode           bsp
        chunkc set global_desktop_offset_top     0
        chunkc set global_desktop_offset_bottom  0
        chunkc set global_desktop_offset_left    0
        chunkc set global_desktop_offset_right   0
        chunkc set global_desktop_offset_gap     0
        chunkc set bsp_spawn_left                1
        chunkc set bsp_optimal_ratio             1.618
        chunkc set bsp_split_mode                optimal
        chunkc set bsp_split_ratio               0.66
        chunkc set window_focus_cycle            all
        chunkc set mouse_follows_focus           1
        chunkc set window_region_locked          1
      '';
    };
  };
  services.skhd = {
    enable = true;
    package = pkgs.skhd;
    skhdConfig = ''
      alt - space : /Applications/kitty.app/Contents/MacOS/kitty --single-instance -d ~

      ctrl + alt + cmd - up    : chunkc tiling::window --warp north
      ctrl + alt + cmd - right : chunkc tiling::window --warp east
      ctrl + alt + cmd - down  : chunkc tiling::window --warp south
      ctrl + alt + cmd - left  : chunkc tiling::window --warp west

      shift + alt + cmd - up    : chunkc tiling::window --focus north
      shift + alt + cmd - right : chunkc tiling::window --focus east
      shift + alt + cmd - down  : chunkc tiling::window --focus south
      shift + alt + cmd - left  : chunkc tiling::window --focus west

      ctrl + alt + cmd - e : chunkc tiling::desktop --equalize
      ctrl + alt + cmd - f : chunkc tiling::window --toggle float
      ctrl + alt + cmd - m : chunkc tiling::window --toggle fullscreen

      shift + cmd - 1 : chunkc tiling::window --send-to-desktop 1
      shift + cmd - 2 : chunkc tiling::window --send-to-desktop 2
      shift + cmd - 3 : chunkc tiling::window --send-to-desktop 3
      shift + cmd - 4 : chunkc tiling::window --send-to-desktop 4
      shift + cmd - 5 : chunkc tiling::window --send-to-desktop 5
      shift + cmd - 6 : chunkc tiling::window --send-to-desktop 6
    '';
  };
  system.defaults = {
    NSGlobalDomain = {
      AppleKeyboardUIMode = 3;

      ApplePressAndHoldEnabled = false;
      InitialKeyRepeat = 10;
      KeyRepeat = 1;

      NSAutomaticDashSubstitutionEnabled = false;
      NSAutomaticQuoteSubstitutionEnabled = false;
      NSAutomaticSpellingCorrectionEnabled = true;

      NSNavPanelExpandedStateForSaveMode = true;
      NSNavPanelExpandedStateForSaveMode2 = true;
    };
    dock = {
      autohide = true;
      mru-spaces = false;
      orientation = "right";
      showhidden = true;
    };
    finder = {
      AppleShowAllExtensions = true;
      FXEnableExtensionChangeWarning = false;
      QuitMenuItem = true;
    };
    trackpad.Clicking = true;
  };
}
