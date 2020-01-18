{ lib, pkgs, ... }:

{

  programs.fish = let inherit (lib.strings) fileContents; in {
    enable = true;
    interactiveShellInit = fileContents ./fish/interactiveShellInit.fish;
    promptInit = ''
      ${fileContents ./fish/sushi/fish_prompt.fish}

      ${fileContents ./fish/sushi/fish_right_prompt.fish}
    '';
    shellInit = fileContents ./fish/shellInit.fish;

    shellAliases = rec {
      ag = rgs;
      agn = rgn; # "ag --nogroup";
      agq = rgf; # "ag -Q";
      e = "${pkgs.emacs}/bin/emacsclient -na \"\"";
      ec = e + " -c";
      et = "${pkgs.emacs}/bin/emacsclient -nw -a \"\"";
      rgn = "rg --no-heading";
      rga = "rg --hidden --iglob !.git";
      rgi = "rg -i";
      rgf = "rg -F";
      rgs = "rg -S";
      g = "git";
      gd = "${g} d";
      gdc = "${g} dc";
      gpg = "gpg2";
      gs = "${g} st";
      k = "clear";
      kc = "kubectl";
      kt = "kubetail";
      l = "ls -Glah";
      ll = "ls -Glh";
      ls = "ls -G";
      # nb = "nix build -f '<nixpkgs>' --no-link";
      # Taskwarrior aliases
      p = "task list";
      pp = tbd;
      t = "task";
      ta = "task add";
      tbd = "task burndown.daily";
      te = "env VISUAL=$EDITOR task edit";
      tm = "task mod";
    };

  };

}
