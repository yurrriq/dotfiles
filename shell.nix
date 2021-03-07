{ pkgs ? import <nixpkgs> }:
let
  pkg = import ./. { inherit pkgs; };
in
pkgs.mkShell {
  inherit (pkg) FONTCONFIG_FILE;
  buildInputs = pkg.nativeBuildInputs ++ (
    with pkgs; (
      [
        biber
        cargo
        git
        gnumake
        gnupg
        python3
        mkpasswd
        niv
        nixpkgs-fmt
        nodePackages.node2nix
        shellcheck
        shfmt
        sops
        stow
      ]
    ) ++ (
      with python3Packages; [
        pre-commit
        pygments
        yamllint
      ]
    )
  );
}
