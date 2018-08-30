{ pkgs ? import <nixpkgs> {} }:

{
  # FIXME
  repos.yurrriq =
    let
      homePath = if pkgs.stdenv.isDarwin then "Users" else "home";
      username = "yurrriq";
    in
      import (builtins.toPath "/${homePath}/${username}/.config/nurpkgs") { inherit pkgs; };

}
