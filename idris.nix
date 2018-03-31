{ config, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      idris
    ];
  };  
}
