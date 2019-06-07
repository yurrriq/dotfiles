{ lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; ([
    aspell
    aspellDicts.en
    autojump
    awscli
    bat
    cachix
    # FIXME: clang
    coreutils
    # TODO: coq
    # TODO: cquery
    curl
    direnv
    fzf
    # gap
    gawk
    gcc
    git
    # git-lfs
    gnumake
    gnupg
    gnused
    gnutar
    graphviz
    httpie
    # TODO: http-promt
    # TODO: idris
    jq
    # TODO: lean
    moreutils
    nix
    noweb
    pandoc
    # TODO: pup
    ripgrep
    shellcheck
    shfmt
    silver-searcher # TODO: find helm-rg solution
    # sloccount
    spotify
    stow
    taskwarrior
    tree
    # vim
    watch
    yq
  ] ++ (with beam.packages.erlangR20; [
    # rebar3-open
  ]) ++ (with gitAndTools; [
    git-crypt
    gitflow
    hub
    lab
  ]) ++ (with nodePackages; [
    # TODO: bash-language-server
    diff-so-fancy
  ]) ++ (with python2Packages; [
    # gap-pygments-lexer
    pywatchman
  ]) ++ (with python3Packages; [
    pygments
  ]) ++ lib.optionals stdenv.isDarwin ([
    clementine
    diff-pdf
    kitty
    m-cli
    sourcetree
    onyx
  ] ++ (with chunkwm; [
    core
    ffm
    tiling
  ])) ++ lib.optionals stdenv.isLinux [
    git-cola
    google-chrome-dev
    # libreoffice
    keybase
    qpdfview
    psmisc
    terminator
    (texlive.combine {
      inherit (texlive) scheme-full tufte-latex;
      inherit noweb;
    })
    # thunderbird
  ]);

}
