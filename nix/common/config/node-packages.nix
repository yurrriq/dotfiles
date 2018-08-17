{ pkgs, ... }:

{

  nixpkgs.config.packageOverrides = super: let self = super.pkgs; in {
    nodejs = super.nodejs-8_x;
    nodePackages = super.nodePackages //
      super.callPackage ../pkgs/development/node-packages {
      inherit (super) pkgs;
      inherit (self) nodejs;
    };
  };

}
