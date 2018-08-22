{ pkgs ? import <nixpkgs> {} }:

{

  repos.yurrriq = import ../../../../nur-packages { inherit pkgs; };

}
