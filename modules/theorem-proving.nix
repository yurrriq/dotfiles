{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    coq
    idris
    lean
  ];
}
