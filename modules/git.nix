{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    git
    git-cola
    git-crypt
    gitAndTools.gitflow
    gitAndTools.hub
    gnupg
  ];

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

}
