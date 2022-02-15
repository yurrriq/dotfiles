{ lib, pkgs, ... }:

{
  home.packages = with pkgs; (
    [
      delta
      diff-pdf
      git
      git-lfs
      github-cli
      kdiff3
      nix-prefetch-git
      nix-prefetch-github
      sops
    ] ++ (
      with gitAndTools; [
        git-extras
        gita
        hub
        lab
      ]
    )
  );

}
