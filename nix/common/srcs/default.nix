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

  _darwin = fetchTarballFromGitHub (fromJSONFile ./darwin.json);

  _nixpkgs = fetchTarballFromGitHub (fromJSONFile ./nixpkgs.json);

  _nur = if local
           then ./local-nur.nix
           else fetchTarballFromGitHub (fromJSONFile ./nur.json);

  nur-no-pkgs = import _nur { };

}
