{ config, pkgs, ... }:

{
  environment = {
    pathsToLink = [
      "/lib/aspell"
      "/share/emacs"
    ];

    systemPackages = with pkgs; [
      aspell
      # aspellDicts.de
      aspellDicts.en
      aspellDicts.es
      # aspellDicts.fr
      # aspellDicts.it
      aspellDicts.sv
      emacs
    ];
  };
}
