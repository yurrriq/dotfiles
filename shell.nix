{
  sources ? import ./nix/sources.nix {},
  pkgs ? import sources.nixpkgs {
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
  buildInputs = pkg.buildInputs ++ (with pkgs; [
    git
    gnumake
    gnupg
    mkpasswd
    sops
    stow
  ]);
}
