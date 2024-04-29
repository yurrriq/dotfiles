{ pkgs, ... }:

{
  home.packages = with pkgs; (
    [
      diff-pdf
      dyff
      git
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
