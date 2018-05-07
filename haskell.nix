{ config, pkgs, ... }:

{
  environment.systemPackages = (with pkgs; [
    stack
  ]) ++ (with pkgs.haskellPackages; [
    hindent
    hpack
    # intero
    pandoc
    pointfree
    pointful
    stylish-haskell
    # titlecase
  ]);
}
