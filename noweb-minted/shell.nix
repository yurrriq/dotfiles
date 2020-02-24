{ pkgs ? import ./nix }:

pkgs.mkShell {
  buildInputs =
    with pkgs; (
      [
        noweb
        cargo
        # git
        gnumake
        gnupg
        python3
        shellcheck
        shfmt
      ]
    ) ++ (
      with python3Packages; [
        pre-commit
        pygments
        yamllint
      ]
    );
}
