{ config, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
#      haskellPackages.idris
    ];
  };
}
