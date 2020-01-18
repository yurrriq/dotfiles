{ lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; ([
    aspell
    aspellDicts.en
    autojump
    awscli
    bind
    cachix
    coreutils
    # TODO: cquery
    curl
    diff-pdf
    expect
    fzf
    gawk
    gcc
    git
    git-lfs
    gnumake
    gnused
    gnutar
    # TODO: graphviz
    gzip
    httpie
    # TODO: http-prompt
    # TODO: idris
    jq
    kdiff3
    kitty
    moreutils
    nix
    nix-prefetch-git
    nix-prefetch-github
    noweb
    pandoc
    prettyping
    ripgrep
    shellcheck
    shfmt
    silver-searcher # TODO: find helm-rg solution
    slack
    sops
    spotify
    stow
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
    unzip
    vim
    watch
    # wipe
    yq
  ]) ++ (with gitAndTools; [
    git-crypt
    # gitflow
    hub
    lab
  ]) ++ (with haskellPackages; [
      cabal-install
      cabal2nix
      hpack
      stylish-haskell
  ]) ++ (with nodePackages; [
    # TODO: bash-language-server
    codeowners
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
    firefox
    # git-cola
    gnome3.networkmanagerapplet
    hicolor-icon-theme
    keybase-gui
    libreoffice
    qpdfview
    playerctl
    psmisc
    (signal-desktop.override {
      spellcheckerLanguage = "en_US";
    })
    thunderbird
    volumeicon
    xclip
    xorg.xbacklight
  ];

}
