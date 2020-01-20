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

  inherit (sources) home-manager;

  nur = if local then ./local-nur.nix else sources.nur;

  nur-no-pkgs = import nur { };

}) // (if seemsDarwin then {

  inherit (sources) darwin;

  nixpkgs = sources.nixpkgs-darwin;

} else {

  inherit (sources) nixpkgs;

})
