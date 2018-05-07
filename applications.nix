{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # FIXME: calibre
    clementine
    # FIXME: frescobaldi
    # FIXME: gnucash
    # FIXME: kindlegen
    musescore
    # FIXME: skim
  ];

  nixpkgs.config.packageOverrides = super: rec {
    clementine = super.callPackage ./pkgs/applications/audio/clementine {};
    gnucash = super.callPackage ./pkgs/applications/office/gnucash {};
    musescore = super.callPackage ./pkgs/applications/audio/musescore/darwin.nix {};
    skim = super.callPackage ./pkgs/applications/misc/skim {};
  };
}
