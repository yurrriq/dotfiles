{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; ([
    autojump
    awscli
    clang
    coreutils
    curl
    direnv
    erlang
    fzf
    gap
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
    jq
    moreutils
    ripgrep
    rlwrap
    silver-searcher
    sloccount
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
    vmd
  ]) ++ (with python27Packages; [
    gap-pygments-lexer
    pygments
    pywatchman
  ]) ++ (with python36Packages; [
    pygments
  ]) ++ lib.optionals stdenv.isDarwin [
    clementine
    diff-pdf
    sourcetree
    onyx
    spotify
  ] ++ lib.optionals stdenv.isLinux [
  ]);

}
