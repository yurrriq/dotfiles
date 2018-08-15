{ pkgs, ... }:

let

  gitGUI = with pkgs; if stdenv.isDarwin then sourcetree else git-cola;

in

{

  environment.systemPackages = (with pkgs; [
    git
    git-crypt
    git-lfs
    gitGUI
    gnupg
  ]) ++ (with pkgs.gitAndTools; [
    gitflow
    hub
  ]) ++ (with pkgs.nodePackages; [
    diff-so-fancy
  ]);

  nixpkgs.config.packageOverrides = super: {
    git-crypt = super.callPackage ../pkgs/applications/version-management/git-and-tools/git-crypt {};
    sourcetree = super.callPackage ../pkgs/os-specific/darwin/sourcetree {};
  };

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

}
