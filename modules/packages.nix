{ lib, pkgs, ... }:
let
  nixpkgs-unstable = import (import ../nix/sources.nix).nixpkgs-unstable {
    config.allowUnfree = true;
  };
in
{
  environment.systemPackages = with pkgs; (
    [
      aspell
      aspellDicts.en
      awscli
      bind
      cachix
      coreutils
      # TODO: cquery
      curl
      expect
      gawk
      gcc
      gnumake
      gnused
      gnutar
      # TODO: graphviz
      gzip
      httpie
      # TODO: http-prompt
      # TODO: idris
      moreutils
      neofetch
      nix
      noweb
      pandoc
      prettyping
      ripgrep
      shellcheck
      shfmt
      silver-searcher # TODO: find helm-rg solution
      slack
      spotify
      stow
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
    with haskellPackages; [
      cabal-install
      cabal2nix
      hpack
      # FIXME: stylish-haskell
    ]
  ) ++ (
    with nodePackages; [
      # TODO: bash-language-server
    ]
  ) ++ (
    with python3Packages; [
      pygments
    ]
  ) ++ lib.optionals stdenv.isDarwin (
    [
      clementine
      m-cli
      sourcetree
      onyx
    ] ++ (
      with chunkwm; [
        core
        ffm
        tiling
      ]
    )
  ) ++ lib.optionals stdenv.isLinux [
    gnome3.networkmanagerapplet
    hicolor-icon-theme
    keybase-gui
    libreoffice
    qpdfview
    playerctl
    psmisc
    (
      nixpkgs-unstable.signal-desktop.override {
        spellcheckerLanguage = "en_US";
      }
    )
    thunderbird
    volumeicon
    xclip
    xorg.xbacklight
  ];

}
