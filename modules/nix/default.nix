{ local ? false, sources ? import ./sources.nix }:

let

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
