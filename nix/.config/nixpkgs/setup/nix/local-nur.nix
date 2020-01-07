{ pkgs ? import <nixpkgs> {} }:

(import (import <setup/nix> { local = false; })._nur { inherit pkgs; }) // {

  repos.yurrriq = import <nurpkgs> { inherit pkgs; };

}
