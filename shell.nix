{ config, pkgs, ... }:

{
  environment = {
    shellAliases = {
      agn = "ag --nogroup";
      agq = "ag -Q";
      k = "clear";
      l = "ls -Glah";
      ll = "ls -Glh";
      ls = "ls -G";
    };

    systemPackages = with pkgs; ([
      autojump
      htop
      httpie
      keybase
      psmisc
      silver-searcher
      tree
    ]) ++ (with python35Packages; [
      pygments
    ]);
  };
  
  programs = {
    bash.enableCompletion = true;

    fish = {
      enable = true;
      shellInit = pkgs.stdenv.lib.strings.fileContents ./shellInit.fish;
    };

    # TODO
    # tmux.enable = true;
  };

  security = {
    sudo.extraConfig = ''
      yurrriq ALL=(ALL) NOPASSWD: ALL
    '';
  };
  
}
