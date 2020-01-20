{ lib, pkgs, ... }:

{

  home.sessionVariables = {
    SHELL = "fish";
    TERMINAL = "kitty";
  };

  programs.fish = let inherit (lib.strings) fileContents; in {
    enable = true;

    interactiveShellInit = fileContents ./interactiveShellInit.fish;

    promptInit = ''
      ${fileContents ./sushi/fish_prompt.fish}

      ${fileContents ./sushi/fish_right_prompt.fish}
    '';

    shellAliases = rec {
      e = "emacsclient -na \"\"";
      ec = e + " -c";
      et = "emacsclient -nw -a \"\"";

      gpg = "gpg2";

      k = "clear";

      l = "ls -Glah";
      ll = "ls -Glh";
      ls = "ls -G";
    };

    shellAbbrs = rec {
      # Old Darwin habits
      pbcopy = "xclip -sel clipboard";
      pbpaste = "${pbcopy} -o";

      # Git
      g = "git";
      gd = "${g} d";
      gdc = "${g} dc";
      gs = "${g} st";

      # Kubernetes
      kc = "kubectl";
      kt = "kubetail";

      # Nix
      nb = "nix build";
      nbn = "${nb} --no-link";

      # ripgrep
      rga = "rg --hidden --iglob !.git";
      rgf = "rg -F";
      rgi = "rg -i";
      rgn = "rg --no-heading";
      rgs = "rg -S";

      # Taskwarrior
      p = "task list";
      pp = tbd;
      t = "task";
      ta = "task add";
      tbd = "task burndown.daily";
      te = "env VISUAL=$EDITOR task edit";
      tm = "task mod";

      trea = "tree -a";
    };

    shellInit = fileContents ./shellInit.fish;
  };

}
