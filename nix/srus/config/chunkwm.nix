{ pkgs, ... }:

{

  environment.systemPackages = with pkgs.chunkwm; [
    core
    ffm
    tiling
  ];

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
      dir  = "/run/current-system/sw/bin/chunkwm-plugins/";
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

}
