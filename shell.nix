{ sources ? import ./modules/nix {}
, pkgs ? import sources.nixpkgs {}
}:

pkgs.mkShell {
  buildInputs = with pkgs; [
    gnumake
    gnupg
    sops
    stow
  ];
}
