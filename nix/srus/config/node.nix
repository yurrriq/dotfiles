{ pkgs, ... }:

{

  environment.systemPackages = with pkgs.nodePackages; [
    json-minify
    json-schema-js-gui-model
    node2nix
    vmd
  ];

}
