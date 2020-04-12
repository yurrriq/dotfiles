{ ... }:

{

  programs.kitty = {
    enable = true;
    config = {
      editor = ''emacsclient -nw -a ""'';
      scrollbackLines = -1;
      term = "xterm";
    };
  };

}
