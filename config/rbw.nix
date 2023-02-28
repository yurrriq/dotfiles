{ pkgs, ... }:

{
  home.packages = with pkgs; [
    rofi-rbw
    xdotool
  ];

  programs.rbw = {
    enable = true;
    settings = {
      email = "eric@ericb.me";
      lock_timeout = 600;
    };
  };
}
