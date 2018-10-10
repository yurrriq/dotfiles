{ pkgs ? import <nixpkgs> {} }:

(import (import <setup/srcs> { local = false; })._nur { inherit pkgs; }) // {
  repos.yurrriq =
    let
      homePath = if pkgs.stdenv.isDarwin then "Users" else "home";
      username = pkgs.lib.maybeEnv "USER" "yurrriq";
    in
      import (builtins.toPath "/${homePath}/${username}/.config/nurpkgs") { inherit pkgs; };

}
