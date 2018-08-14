{ pkgs, ... }:

{

  environment.systemPackages = (with pkgs; [
    git
    git-crypt
    git-lfs
  ] ++ (with gitAndTools; [
    gitflow
    hub
  ]) ++ (with nodePackages; [
    diff-so-fancy
  ]));

  nixpkgs.config.packageOverrides = super: {
    git-crypt = super.callPackage ../pkgs/applications/version-management/git-and-tools/git-crypt {};
  };

}
