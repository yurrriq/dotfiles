{ pkgs ? import ./nix/nixpkgs.nix }:
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
