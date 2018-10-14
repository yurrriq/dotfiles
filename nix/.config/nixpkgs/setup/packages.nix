{ lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; ([
    aspell
    aspellDicts.en
    autojump
    awscli
    clang
    coreutils
    # TODO: coq
    curl
    direnv
    fzf
    # gap
    gawk
    gcc
    git
    git-lfs
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
    # TODO: pup
    ripgrep
    rlwrap
    shellcheck
    shfmt
    silver-searcher # TODO: find helm-rg solution
    sloccount
    spotify
    stow
    tree
    watch
  ] ++ (with beam.packages.erlangR20; [
    rebar3-open
  ]) ++ (with gitAndTools; [
    git-crypt
    gitflow
    hub
    lab
  ]) ++ (with nodePackages; [
    diff-so-fancy
    json-minify
    node2nix
  ]) ++ (with python2Packages; [
    # gap-pygments-lexer
    pywatchman
  ]) ++ (with python3Packages; [
    pygments
  ]) ++ lib.optionals stdenv.isDarwin [
    clementine
    diff-pdf
    sourcetree
    onyx
  ] ++ lib.optionals stdenv.isLinux [
    git-cola
    google-chrome
    libreoffice
    keybase
    noweb
    psmisc
    terminator
    (texlive.combine {
      inherit (texlive) scheme-full tufte-latex;
    })
    thunderbird
  ]);

}
