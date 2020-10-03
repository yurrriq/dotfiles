{ pkgs ? import ../../nix/nixpkgs.nix }:
let
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
in
pkgs.haskellPackages.callCabal2nix "my-xmonad" src { }
