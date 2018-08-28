{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    # FIXME: calibre
    clementine
    # FIXME: frescobaldi
    # FIXME: gnucash
    # FIXME: kdiff3
    # FIXME: kindlegen
    onyx
    spotify
  ];

}
