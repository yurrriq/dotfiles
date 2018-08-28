{ ... }:

{

  programs.tmux = {
    enable = true;
    iTerm2 = true;
    tmuxConfig = ''
      set -s escape-time 0
    '';
  };

}
