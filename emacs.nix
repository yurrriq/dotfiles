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
      e = "ec";
      ec = ''emacsclient -cna ""'';
      et = ''emacsclient -cnw -a ""'';
    };
  };

  services = {
    emacs = {
      enable = true;
      inherit package;
    };
  };
}
