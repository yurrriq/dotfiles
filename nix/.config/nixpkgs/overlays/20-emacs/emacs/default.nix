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
  mmm-mode
]) ++ (with melpa; [
  avy
  better-defaults
  clj-refactor
  clojure-mode
  # TODO: company-lsp
  # TODO: cquery
  crux
  dhall-mode
  direnv
  dockerfile-mode
  editorconfig
  elixir-mode
  enh-ruby-mode
  # erlang
  fill-column-indicator
  fish-mode
  # gap-mode
  go-mode
  graphviz-dot-mode
  haskell-mode
  helm-ag
  hindent
  hl-todo
  htmlize
  idris-mode
  kubernetes-tramp
  # TODO: lsp-haskell
  # TODO: lsp-mode
  # TODO: lsp-ui
  magit
  markdown-mode
  multiple-cursors
  nix-mode
  paredit
  rainbow-delimiters
  robe
  rust-mode
  rvm
  smex
  tuareg
  use-package
  whitespace-cleanup-mode
  yaml-mode
]) ++ (with melpaStable; [
  ess
]) ++ (with org; [
]))
