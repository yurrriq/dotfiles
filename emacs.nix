{ config, pkgs, ... }:

let

  package = pkgs.emacsWithPackages (epkgs: with {
    elpa = epkgs.elpaPackages;
    melpa = epkgs.melpaPackages;
    melpaStable = epkgs.melpaStablePackages;
    org = epkgs.orgPackages;
  }; (with elpa; [
  ]) ++ (with melpa; [
    fish-mode
    idris-mode    
    magit
    nix-mode
    paredit
  ]) ++ (with melpaStable; [
  ]) ++ (with org; [
  ]) ++ [
  ]);

in

{
  environment = {
    shellAliases = {
      e = ''emacsclient -cna ""'';
      et  = ''emacsclient -cta ""'';
    };

    systemPackages = with pkgs; [
      aspell
    ] ++ (with aspellDicts; [
      en
    ]);
  };
  
  services = {
    emacs = {
      enable = true;
      inherit package;
    };
  };
}
