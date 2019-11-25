{ local ? false, sources ? import ./sources.nix }:

let

  fetchTarballFromGitHub =
    { owner, repo, rev, sha256, ... }:
    builtins.fetchTarball {
      url = "https://github.com/${owner}/${repo}/tarball/${rev}";
      inherit sha256;
    };

  fromJSONFile = f: builtins.fromJSON (builtins.readFile f);

  seemsDarwin = null != builtins.match ".*darwin$" builtins.currentSystem;

in

(rec {

  _nur =
    if local then
      { pkgs ? import <nixpkgs> {} }:
      (import sources.nur { inherit pkgs; }) // {
      repos.yurrriq =
        let
          homePath = if pkgs.stdenv.isDarwin then "Users" else "home";
          username =
            if pkgs.stdenv.isDarwin then
              pkgs.lib.maybeEnv "USER" "yurrriq"
            else
              "e.bailey";
        in
        import (builtins.toPath "/${homePath}/${username}/.config/nurpkgs") {
          inherit pkgs;
        };
      }
    else
      sources.nur;

  nur-no-pkgs = import _nur { };

}) // (if seemsDarwin then {

  _darwin = fetchTarballFromGitHub (fromJSONFile ./darwin.json);

  _nixpkgs = fetchTarballFromGitHub (fromJSONFile ./nixpkgs-darwin.json);

} else {

  _nixpkgs = sources.nixpkgs;

})
