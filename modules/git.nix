{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; ([
    git
    git-cola
    git-crypt
    gnupg
  ] ++ (with pkgs.gitAndTools; [
    diff-so-fancy
    gitflow
    hub
  ]));

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

}
