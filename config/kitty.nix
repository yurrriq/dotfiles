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
    url = "https://raw.githubusercontent.com/dexpota/kitty-themes/c4bee86c/themes/Wombat.conf";
    hash = "sha256-macm9bb/9zWLeFANXqiYPc5IS40A7ZbhXr/DooJARsQ=";
    postFetch = ''
      ${pkgs.gawk}/bin/gawk -i inplace '
          /^background/           { sub($2, "#242424") }
          /^foreground/           { sub($2, "#f6f3e8") }
          /^cursor/               { sub($2, "#656565") }
          /^selection_background/ { sub($2, "#444444") }
          /^color0/               { sub($2, "#242424") }
          /^color8/               { sub($2, "#303030") }
          /^selection_foreground/ { sub($2, "#f6f3e8") }
          { print }
      ' $out
    '';
  };

}
