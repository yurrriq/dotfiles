{ pkgs, ... }:

{

  environment.systemPackages = with pkgs.nodePackages; [
    json-minify
    node2nix
    vmd
  ];

}
