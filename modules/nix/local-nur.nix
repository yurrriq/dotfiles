{ pkgs ? import <nixpkgs> {} }:

(import (import ./. { local = false; })._nur { inherit pkgs; }) // {

  repos.yurrriq = import "${home.homeDirectory}/src/github.com/yurrriq/nur-packages" { inherit pkgs; };

}
