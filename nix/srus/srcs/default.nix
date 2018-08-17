let

  fetchTarballFromGitHub =
    { owner, repo, rev, sha256, ... }:
    builtins.fetchTarball {
      url = "https://github.com/${owner}/${repo}/tarball/${rev}";
      inherit sha256;
    };

  fromJSONFile = f: builtins.fromJSON (builtins.readFile f);

in

{
  _nixpkgs = fetchTarballFromGitHub (fromJSONFile ./nixpkgs.json);

  _nur = fetchTarballFromGitHub (fromJSONFile ./nur.json);

}
