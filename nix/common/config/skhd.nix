{ pkgs, ... }:

{

  services.skhd = {

    enable = true;

    package =  pkgs.skhd;

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

      ctrl + alt + cmd - m : chunkc tiling::window --toggle fullscreen

      shift + cmd - 1 : chunkc tiling::window --send-to-desktop 1
      shift + cmd - 2 : chunkc tiling::window --send-to-desktop 2
      shift + cmd - 3 : chunkc tiling::window --send-to-desktop 3
      shift + cmd - 4 : chunkc tiling::window --send-to-desktop 4
      shift + cmd - 5 : chunkc tiling::window --send-to-desktop 5
      shift + cmd - 6 : chunkc tiling::window --send-to-desktop 6
    '';

  };

}
