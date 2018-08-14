{ config, pkgs, ... }:

{

  environment = {

    pathsToLink = [
      "/share/gap"
    ];

    systemPackages = with pkgs; [
      gap
    ];

  };

  nixpkgs.config.packageOverrides = super: {
    # TODO: gap = super.callPackage ../pkgs/applications/science/math/gap {};
  };

}
