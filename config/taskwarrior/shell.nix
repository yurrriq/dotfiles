{ pkgs ? import ./src/nix/nixpkgs.nix }:

let
  pkg = import ./src/nix { inherit pkgs; };
in

pkgs.mkShell {
  inherit (pkg) FONTCONFIG_FILE;
  buildInputs = [
    pkgs.nix-prefetch-github
  ] ++ pkg.buildInputs;
}
