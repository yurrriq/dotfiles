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
    lab
  ]) ++ (with pkgs.gitAndTools; [
    gitflow
    hub
  ]) ++ (with pkgs.nodePackages; [
    diff-so-fancy
  ]);

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

}
