{ pkgs ? import <nixpkgs> {} }:

let

  myEmacs = pkgs.emacs;

  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;

in

emacsWithPackages (epkgs: with {
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
  fill-column-indicator
  fish-mode
  helm-ag
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
]))
