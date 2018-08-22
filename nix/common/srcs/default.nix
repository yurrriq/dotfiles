{ local ? false }:


let

  fetchTarballFromGitHub =
    { owner, repo, rev, sha256, ... }:
    builtins.fetchTarball {
      url = "https://github.com/${owner}/${repo}/tarball/${rev}";
      inherit sha256;
    };

  fromJSONFile = f: builtins.fromJSON (builtins.readFile f);

in

rec {

  _nixpkgs = fetchTarballFromGitHub (fromJSONFile ./nixpkgs.json);

  _nur = if local
           then fetchTarballFromGitHub (fromJSONFile ./nur.json)
           else ./local-nur.nix;

  nur-no-pkgs = import _nur { };

}
