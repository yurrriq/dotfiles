{ config, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      file
      libqrencode
      lsof
      steghide
      tomb
    ];
  };    
}
