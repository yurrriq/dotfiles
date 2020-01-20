{ lib, pkgs, ... }:

{
  home.packages = with pkgs; ([
    git
    git-lfs
    kdiff3
    nix-prefetch-git
    nix-prefetch-github
    sops
  ] ++ (lib.optionals pkgs.stdenv.isLinux [
    # git-cola
  ]) ++ (with gitAndTools; [
    git-crypt
    # gitflow
    hub
    lab
  ]) ++ (with nodePackages; [
    codeowners
    diff-so-fancy
  ]));

}
