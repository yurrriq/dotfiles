{ config, pkgs, stdenv, ... }:

{
  environment = {
    loginShell = "${pkgs.fish}/bin/fish";

    pathsToLink = [
      "/share/autojump"
      # "/share/cows"
    ];

    shellAliases = {
      agn = "ag --nogroup";
      agq = "ag -Q";
      e   = "ec";
      ec  = ''emacsclient -cna ""'';
      et  = ''emacsclient -cnw -a ""'';
      gpg = "gpg2";
      k   = "clear";
      l   = "ls -Glah";
      ll  = "ls -Glh";
      ls  = "ls -G";
    };

    systemPackages = with pkgs; ([
      autojump
      # automake
      awscli
      # cowsay
      curl
      coreutils
      direnv
      exercism
      # fpp
      gawk
      gnumake
      gnused
      gnutar
      # highlight
      html-tidy
      htop
      httpie
      indent
      # TODO: jid
      jq
      literate
      m-cli
      moreutils
      # mosh
      # ngrok # TODO: 2.x
      openssh
      # p7zip
      rlwrap
      rsync
      silver-searcher
      # sloccount
      # FIXME: sshfs-fuse
      tree
      watch
      watchman
      # wakatime
      # wget
      xorg.lndir
    ]) ++ (with pkgs; with python27Packages; [
      pygments
      pygmentsGAP
      pywatchman
    ]) ++ (with python36Packages; [
      pygments
    ]);
  };

  programs = {
    fish = {
      enable = true;
      shellInit = pkgs.stdenv.lib.strings.fileContents ./shellInit.fish;
    };

    tmux = {
      enable = true;
      iTerm2 = true;
      tmuxConfig = ''
        set -s escape-time 0
      '';
    };
  };

  nixpkgs.config.packageOverrides = super: let self = super.pkgs; in rec {
    autojump = super.callPackage ./pkgs/tools/misc/autojump {};
    literate = super.callPackage ./pkgs/development/tools/literate-programming/literate {};
    m-cli = super.callPackage ./pkgs/tools/misc/m-cli {};
    wakatime = super.callPackage ./pkgs/tools/misc/wakatime {};
  };
}
