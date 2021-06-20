{
  description = "My XMonad config";

  inputs = {
    dotfiles.url = "path:../..";
    emacs-overlay.follows = "dotfiles/emacs-overlay";
    nixpkgs.follows = "dotfiles/nixpkgs";
    nixpkgs-unstable.follows = "dotfiles/nixpkgs-unstable";
  };

  outputs = { self, dotfiles, emacs-overlay, ... }:
    let
      pkgs = import dotfiles.inputs.nixpkgs-unstable {
        overlays = [
          emacs-overlay.overlay
          self.overlay
        ];
        system = "x86_64-linux";
      };
    in
    {
      overlay = final: prev: {
        myEmacs = prev.emacsWithPackagesFromUsePackage {
          alwaysEnsure = true;
          config = ./emacs.el;
        };
      };

      defaultPackage.x86_64-linux = self.packages.x86_64-linux.my-xmonad;

      devShell.x86_64-linux = pkgs.mkShell {
        buildInputs = with pkgs; [
          cabal-install
          ghcid
          gitAndTools.pre-commit
          haskell-language-server
          haskellPackages.ormolu
          haskellPackages.pointfree
          myEmacs
        ] ++ self.defaultPackage.x86_64-linux.env.nativeBuildInputs;
      };

      packages.x86_64-linux.my-xmonad =
        pkgs.haskellPackages.callCabal2nix "my-xmonad" ./. { };
    };
}
