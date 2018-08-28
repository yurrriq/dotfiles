{ config, pkgs, ... }:

{

  imports = [
    ./fish.nix
    ./tmux.nix
  ];


  environment = {

    shellAliases = rec {
      ag = rgs;
      agn = rgn; # "ag --nogroup";
      agq = rgf; # "ag -Q";
      rgn = "rg --no-heading";
      rga = "rg --hidden --iglob !.git";
      rgi = "rg -i";
      rgf = "rg -F";
      rgs = "rg -S";
      e = ''emacsclient -na ""'';
      ec = e + "-c";
      et = ''emacsclient -nw -a ""'';
      gpg = "gpg2";
      k = "clear";
      kc = "kubectl";
      l = "ls -Glah";
      ll = "ls -Glh";
      ls = "ls -G";
    };

    systemPackages = with pkgs; ([
      autojump
      awscli
      curl
      coreutils
      direnv
      exercism
      fzf
      gawk
      gnumake
      gnused
      gnutar
      httpie
      jq
      m-cli
      moreutils
      ripgrep
      rlwrap
      silver-searcher
      sloccount
      stow
      tree
      watch
    ]) ++ (with pkgs; with python27Packages; [
      pywatchman
    ]) ++ (with python36Packages; [
      pygments
    ]);

  };

}
