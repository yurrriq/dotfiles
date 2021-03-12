{ lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; (
    [
      aspell
      aspellDicts.en
      awscli
      bind
      cachix
      clac
      clementine
      coreutils
      # TODO: cquery
      curl
      expect
      gawk
      # gcc
      gnumake
      gnused
      gnutar
      gzip
      httpie
      # TODO: http-prompt
      # TODO: idris
      lorri # TODO: remove
      moreutils
      # neofetch
      nixUnstable
      noweb
      # pandoc
      # prettyping
      ripgrep
      shellcheck
      shfmt
      silver-searcher # TODO: find helm-rg solution
      slack
      spotify
      # stow
      (
        texlive.combine {
          inherit (texlive) scheme-small
            datetime
            dirtytalk
            fmtcount
            framed
            fvextra
            hardwrap
            ifplatform
            latexmk
            minted
            titlesec
            todonotes
            tufte-latex
            xetex
            xstring
            ;
          inherit noweb;
        }
      )
      tree
      unzip
      vim
      watch
      # wipe
      yq
    ]
  ) ++ (
    with python3Packages; [
      pygments
    ]
  ) ++ lib.optionals stdenv.isLinux [
    dolphin
    libnotify
    libreoffice
    lm_sensors
    mtr
    networkmanager-openconnect
    nfs-utils
    openconnect
    file
    lsof
    tomb
    (signal-desktop.override { spellcheckerLanguage = "en_US"; })
    xclip
    xorg.xbacklight
  ];

}
