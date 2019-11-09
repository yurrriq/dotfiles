{ pkgs ? import <nixpkgs> {} }:

(import (import <setup/srcs> { local = false; })._nur { inherit pkgs; }) // {

  repos.yurrriq =
    let
      homePath = if pkgs.stdenv.isDarwin then "Users" else "home";
      username = if pkgs.stdenv.isDarwin
                   then pkgs.lib.maybeEnv "USER" "yurrriq"
                   else "e.bailey";
    in
    import (builtins.toPath "/${homePath}/${username}/.config/nurpkgs") {
      inherit pkgs;
    };

}
