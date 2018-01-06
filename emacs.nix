{ emacsWithPackages, ... }:

(emacsWithPackages (epkgs: with {
  elpa = epkgs.elpaPackages;
  melpa = epkgs.melpaPackages;
  melpaStable = epkgs.melpaStablePackages;
  org = epkgs.orgPackages;
}; (with elpa; [
]) ++ (with melpa; [
  idris-mode
  nix-mode
]) ++ (with melpaStable; [
]) ++ (with org; [
]) ++ [
]))
