{ ... }:

{

  programs.direnv = {
    enable = true;
    enableZshIntegration = false;
    nix-direnv = {
      enable = true;
      enableFlakes = true;
    };
  };

}
