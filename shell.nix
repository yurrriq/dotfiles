{ sources ? import ./nix/sources.nix {}
, pkgs ? import sources.nixpkgs {}
}:

pkgs.mkShell {
  buildInputs = with pkgs; [
    git
    gnumake
    gnupg
    mkpasswd
    sops
    stow
  ];
}
