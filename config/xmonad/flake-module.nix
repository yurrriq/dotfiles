{ ... }:

{
  perSystem = { config, pkgs, self', ... }: {
    devShells.xmonad = pkgs.mkShell {
      inputsFrom = [
        self'.packages.my-xmonad.env
      ];
      nativeBuildInputs = with pkgs; [
        cabal-install
        (
          emacsWithPackagesFromUsePackage {
            alwaysEnsure = true;
            config = ./emacs.el;
          }
        )
        ghcid
        haskell-language-server
        haskellPackages.hlint
        haskellPackages.ormolu
        haskellPackages.pointfree
        nixd
        pre-commit
        xorg.libX11
        xorg.libXScrnSaver
        xorg.libXext
        xorg.libXft
        xorg.libXinerama
        xorg.libXrandr
      ];
    };
    packages.my-xmonad =
      pkgs.haskellPackages.callCabal2nix "my-xmonad" ./. { };
  };
}
