{ config, pkgs, ... }:

let

  myEmacs = pkgs.emacs;

  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;

  package = emacsWithPackages (epkgs: with {
    elpa = epkgs.elpaPackages;
    melpa = epkgs.melpaPackages;
    melpaStable = epkgs.melpaStablePackages;
    org = epkgs.orgPackages;
  }; (with elpa; [
  ]) ++ (with melpa; [
    fish-mode
    hl-todo
    htmlize
    idris-mode
    magit
    nix-mode
    paredit
  ]) ++ (with melpaStable; [
    ess
    monokai-theme
  ]) ++ (with org; [
  ]));

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
