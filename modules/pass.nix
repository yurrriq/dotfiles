{ config, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      pass
      # qtpass
    ];
  };
    
  programs = {
    browserpass.enable = true;
  };
}
