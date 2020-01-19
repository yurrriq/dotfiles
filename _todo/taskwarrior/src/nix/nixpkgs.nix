let
  srcDef = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/${srcDef.owner}/${srcDef.repo}/tarball/${srcDef.rev}";
    inherit (srcDef) sha256;
  };

in

import nixpkgs {
  overlays = [
    (import ./overlays/10-noweb.nix)
    (import ./overlays/11-xelatex.nix)
  ];
}
