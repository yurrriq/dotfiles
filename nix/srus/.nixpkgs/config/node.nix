{ config, lib, pkgs, ... }:

{

  environment.systemPackages = with pkgs.nodePackages; [
    json-minify
    json-schema-js-gui-model
    node2nix
    vmd
  ];

  nixpkgs.config.packageOverrides = super: let self = super.pkgs; in {
    nodejs = super.nodejs-6_x;
    nodePackages = super.nodePackages //
      super.callPackage ../pkgs/development/node-packages {
      inherit (super) pkgs;
      inherit (self) nodejs;
    };
    nodePackages_8_x = super.nodePackages_8_x //
      super.callPackage ../pkgs/development/node-packages-8x {
      inherit (super) pkgs;
      nodejs = super.nodejs-8_x;
    };
  };

}
