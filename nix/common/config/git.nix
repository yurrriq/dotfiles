{ pkgs, ... }:

let

  gitGUI = with pkgs; if stdenv.isDarwin then sourcetree else git-cola;

in

{

  environment.systemPackages = (with pkgs; [
    git
    git-lfs
    gitGUI
    gnupg
  ]) ++ (with pkgs.gitAndTools; [
    git-crypt
    gitflow
    hub
    lab
  ]) ++ (with pkgs.nodePackages; [
    diff-so-fancy
  ]);

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

}
