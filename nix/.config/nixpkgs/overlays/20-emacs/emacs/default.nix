{ pkgs ? import <nixpkgs> {} }:

let
  # TODO
  # structured-haskell-mode = pkgs.emacsPackagesNg.melpaBuild {
  #   pname = "shm";
  #   version = "20170523";
  #   src = pkgs.fetchFromGitHub {
  #     owner = "chrisdone";
  #     repo = "structured-haskell-mode";
  #     rev = "7f9df73f45d107017c18ce4835bbc190dfe6782e";
  #     #date: 2018-03-26T20:57:49-04:00;
  #     sha256 = "1jcc30048j369jgsbbmkb63whs4wb37bq21jrm3r6ry22izndsqa";
  #   };
  #   packageRequires = [pkgs.emacsPackagesNg.haskell-mode ];
  #   fileSpecs = [ "elisp/*.el" ];
  #   propagatedUserEnvPkgs = [pkgs.haskellPackages.structured-haskell-mode ];
  #   recipe = pkgs.fetchurl {
  #     url = "https://raw.githubusercontent.com/milkypostman/melpa/68a2fddb7e000487f022b3827a7de9808ae73e2a/recipes/shm";
  #     sha256 = "1qmp8cc83dcz25xbyqd4987i0d8ywvh16wq2wfs4km3ia8a2vi3c";
  #     name = "recipe";
  #   };
  #   meta = {
  #     description = "Structured editing Emacs mode for Haskell";
  #     license = pkgs.lib.licenses.bsd3;
  #     platforms = pkgs.haskellPackages.structured-haskell-mode.meta.platforms;
  #   };
  # };

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
  # TODO: company-lsp
  crux
  dhall-mode
  erlang
  fill-column-indicator
  fish-mode
  haskell-mode
  helm-ag
  hindent
  hl-todo
  htmlize
  idris-mode
  # TODO: lsp-haskell
  # TODO: lsp-mode
  # TODO: lsp-ui
  magit
  multiple-cursors
  nix-mode
  paredit
  rainbow-delimiters
  # TODO: structured-haskell-mode
  use-package
  whitespace-cleanup-mode
  yaml-mode
]) ++ (with melpaStable; [
  ess
]) ++ (with org; [
]))
