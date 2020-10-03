{ pkgs ? import ../../nix/nixpkgs.nix }:
let
  pkg = import ./release.nix { inherit pkgs; };
in
pkgs.mkShell {
  buildInputs = (
    with pkgs.haskellPackages; [
      ormolu
    ]
  ) ++ pkg.env.nativeBuildInputs;
}
