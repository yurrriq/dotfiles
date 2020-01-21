{ sources ? import ./modules/nix {}
, pkgs ? import sources.nixpkgs {}
}:

pkgs.mkShell {
  buildInputs = with pkgs; [
    git
    gnumake
    gnupg
    sops
    stow
  ];
}
