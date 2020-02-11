{ pkgs ? import ./nix/nixpkgs.nix }:
let
  pkg = import ./. { inherit pkgs; };
in
pkgs.mkShell {
  inherit (pkg) FONTCONFIG_FILE;
  buildInputs = pkg.buildInputs ++ (
    with pkgs; (
      [
        biber
        cargo
        git
        gnumake
        gnupg
        mkpasswd
        shellcheck
        shfmt
        sops
        stow
      ]
    ) ++ (
      with python3Packages; [
        pre-commit
        yamllint
      ]
    )
  );
}
