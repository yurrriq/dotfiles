{ pkgs, ... }:

{

  environment = {

    shellAliases = {
      agn = "ag --nogroup";
      agq = "ag -Q";
      k = "clear";
      l = "ls -Glah";
      ll = "ls -Glh";
      ls = "ls -G";
      rgi = "rg -i";
    };

    systemPackages = with pkgs; ([
      autojump
      direnv
      gnumake
      htop
      # TODO: http-prompt
      httpie
      indent
      jq
      keybase
      noweb
      psmisc
      # TODO: pup
      ripgrep
      silver-searcher
      stow
      (texlive.combine {
        inherit (texlive) scheme-full tufte-latex;
      })
      tree
    ]) ++ (with python3Packages; [
      grip
      pygments
    ]);

  };

  programs = {

    bash.enableCompletion = true;

    fish = {
      enable = true;
      shellInit = pkgs.stdenv.lib.strings.fileContents ./shell/shellInit.fish;
    };

    # TODO: tmux.enable = true;

  };
}
