{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    cabal2nix
    ghc
  ] ++ (with pkgs.haskellPackages; [
    hadolint
    hindent
    hpack
    # FIXME: hpack-convert
    # FIXME: jsons-to-schema
    # FIXME: stack
    stylish-haskell
  ]);

  nixpkgs.config.packageOverrides = super: {
    haskellPackages = super.haskellPackages.override {
      overrides = hself: hsuper: {
        hadolint  = hself.callPackage ../pkgs/development/haskell-modules/hadolint {};
        language-docker = hself.callPackage ../pkgs/development/haskell-modules/language-docker {};
        jsons-to-schema = hself.callPackage ../pkgs/development/haskell-modules/jsons-to-schema {};
      };
    };
  };

}
