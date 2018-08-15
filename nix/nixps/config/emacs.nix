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
    better-defaults
    clj-refactor
    clojure-mode
    crux
    erlang
    fish-mode
    fill-column-indicator
    hl-todo
    htmlize
    idris-mode
    magit
    multiple-cursors
    nix-mode
    paredit
    rainbow-delimiters
    whitespace-cleanup-mode
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

  services.emacs = {
    enable = true;
    inherit package;
  };

}
