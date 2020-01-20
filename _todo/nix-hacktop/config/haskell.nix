{ pkgs, ... }:

{

  environment.systemPackages = (with pkgs; [
    pandoc
    stack
  ]) ++ (with pkgs.haskellPackages; [
    hadolint
    hindent
    hpack
    pointfree
    pointful
    stylish-haskell
    # titlecase
  ]);

}
