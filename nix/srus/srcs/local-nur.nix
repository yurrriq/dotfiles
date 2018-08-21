{ pkgs ? import <nixpkgs> {} }:

with import ./. {};

{

  repos.yurrriq = import ../../../../nur-packages { inherit pkgs; };

}
