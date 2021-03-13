{
  description = "My XMonad config";

  inputs = {
    dotfiles.url = "path:../..";
    nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, dotfiles, ... }:
    let
      pkgs = dotfiles.inputs.nixpkgs-unstable.legacyPackages.x86_64-linux;
    in
    {
      defaultPackage.x86_64-linux = self.packages.x86_64-linux.my-xmonad;

      devShell.x86_64-linux = pkgs.mkShell {
        buildInputs = (
          with pkgs; [
            gitAndTools.pre-commit
          ]
        ) ++ (
          with pkgs.haskellPackages; [
            cabal2nix
            ormolu
            pointfree
          ]
        )
        ++ self.defaultPackage.x86_64-linux.env.nativeBuildInputs;
      };

      packages.x86_64-linux.my-xmonad =
        pkgs.haskellPackages.callCabal2nix "my-xmonad" ./. { };
    };
}
