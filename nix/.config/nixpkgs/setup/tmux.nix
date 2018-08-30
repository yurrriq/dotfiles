{ pkgs, ... }:

{

  programs.tmux = {
    enable = true;
  } // (if pkgs.stdenv.isDarwin then {
    iTerm2 = true;
    tmuxConfig = ''
      set -s escape-time 0
    '';
  } else {});

}
