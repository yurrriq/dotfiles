{ config, lib, pkgs, ... }:

{

  xdg.configFile."dunst/dunstrc".source = ./dunstrc;

}
