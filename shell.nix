{ sources ? import ./nix/sources.nix {}
, pkgs ? import sources.nixpkgs {
    overlays = [
      (import ./nix/overlay.nix)
    ];
  }
}:
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
