{ pkgs, ... }:

{

  programs.tmux = {
    enable = false;
  } // (if pkgs.stdenv.isDarwin then {
    tmuxConfig = ''
      set -s escape-time 0
    '';
  } else {});

}
