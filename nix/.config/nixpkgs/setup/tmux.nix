{ pkgs, ... }:

{

  programs.tmux = {
    enable = true;
  } // (if pkgs.stdenv.isDarwin then {
    tmuxConfig = ''
      set -s escape-time 0
    '';
  } else {});

}
