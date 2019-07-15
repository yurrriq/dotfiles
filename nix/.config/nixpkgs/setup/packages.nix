{ lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; ([
    aspell
    aspellDicts.en
    autojump
    awscli
    bat
    bind
    # FIXME: cachix
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
    # graphviz
    gzip
    httpie
    htop
    # TODO: http-promt
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
    tree
    vim
    watch
    wipe
    yq
  ]) ++ (with gitAndTools; [
    git-crypt
    gitflow
    hub
    lab
  ]) ++ (with nodePackages; [
    # TODO: bash-language-server
    diff-so-fancy
  ]) ++ (with python2Packages; [
    # pywatchman
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
    git-cola
    # TODO: libreoffice
    qpdfview
    psmisc
    # (texlive.combine {
    #   inherit (texlive) scheme-full tufte-latex;
    #   inherit noweb;
    # })
    vivaldi
  ];
}
