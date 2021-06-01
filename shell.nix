{ pkgs ? import <nixpkgs> { }, yurrriq-dotfiles ? pkgs.callPackage ./. { } }:
with pkgs;
mkShell {
  inherit (yurrriq-dotfiles) FONTCONFIG_FILE;
  buildInputs = yurrriq-dotfiles.nativeBuildInputs ++ [
    biber
    cargo
    git
    gitAndTools.pre-commit
    gnumake
    gnupg
    mkpasswd
    nixpkgs-fmt
    nodePackages.node2nix
    python3Packages.pygments
    shellcheck
    shfmt
    sops
    stow
  ];
}
