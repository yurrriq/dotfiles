{ lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; ([
    aspell
    aspellDicts.en
    autojump
    awscli
    # bat
    bind
    # cachix # FIXME: servant hash mismatch
    coreutils
    # TODO: cquery
    curl
    direnv
    gawk
    gcc
    git
    git-lfs
    gnumake
    gnupg
    gnused
    gnutar
    # FIXME: graphviz
    gzip
    httpie
    htop
    # TODO: http-prompt
    # TODO: idris
    jq
    kdiff3
    kitty
    moreutils
    nix
    nix-prefetch-git
    noweb
    pandoc
    # TODO: pup
    ripgrep
    shellcheck
    shfmt
    silver-searcher # TODO: find helm-rg solution
    slack
    sops
    spotify
    stow
    taskwarrior
    tasknc
    (texlive.combine {
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
        xstring;
      inherit noweb;
    })
    tree
    vim
    watch
    # wipe
    yq
  ]) ++ (with gitAndTools; [
    git-crypt
    # gitflow
    hub
    lab
  ]) ++ (with nodePackages; [
    # TODO: bash-language-server
    diff-so-fancy
  ]) ++ (with python3Packages; [
    pygments
  ]) ++ lib.optionals stdenv.isDarwin ([
    clementine
    diff-pdf
    m-cli
    sourcetree
    onyx
  ] ++ (with chunkwm; [
    core
    ffm
    tiling
  ])) ++ lib.optionals stdenv.isLinux [
    # NOTE: https://blorg.ericb.me/2019/10/browserpass-on-nixos/
    browserpass
    # git-cola
    # keybase-gui
    # FIXME: libreoffice
    qpdfview
    playerctl
    psmisc
    (signal-desktop.override {
      spellcheckerLanguage = "en_US";
    })
    # FIXME: thunderbird
    xclip
    xorg.xbacklight
  ];
}
