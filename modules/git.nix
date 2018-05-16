{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    git
    git-crypt
    gitAndTools.gitflow
    gitAndTools.hub
    gitg
    gnupg
  ];

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
  };
}
