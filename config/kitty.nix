{ pkgs, ... }:

{

  programs.kitty = {
    enable = true;
    extraConfig = ''
      include theme.conf
    '';
    font = {
      name = "Iosevka Term";
      package = (pkgs.nerdfonts.override { fonts = [ "Iosevka" ]; });
    };
    keybindings = {
      "kitty_mod+enter" = "new_window_with_cwd";
      "kitty_mod+k" = ''
        combine : clear_terminal scrollback active : send_text normal \x0c
      '';
    };
    settings = {
      editor = ''emacsclient -nw -a ""'';
      font_size = 24;
      kitty_mod = "ctrl+shift";
      scrollback_lines = -1;
      shell = ".";
      term = "xterm";
      window_border_width = 0;
    };
  };

  xdg.configFile."kitty/theme.conf".source = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/dexpota/kitty-themes/c4bee86c/themes/Solarized_Dark.conf";
    hash = "sha256-jkB5Oat50iTnmNDMbbNzAI6IWKFP+U44K6IC8GZIcHY=";
    postFetch = ''
      sed -E -i 's/^(selection_background) #002731$/\1 #708183/' $out
    '';
  };

}
