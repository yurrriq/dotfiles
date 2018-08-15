{ pkgs, ... }:

{

  environment.systemPackages = with pkgs.nodePackages; [
    json-minify
    json-schema-js-gui-model
    node2nix
    vmd
  ];

  nixpkgs.config.packageOverrides = super: let self = super.pkgs; in {
    nodejs = super.nodejs-8_x;
    nodePackages = super.nodePackages //
      super.callPackage ../pkgs/development/node-packages {
      inherit (super) pkgs;
      inherit (self) nodejs;
    };
  };

}
