{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    jdk
  ];

  nixpkgs.config.packageOverrides = super: {
    jdk = super.openjdk8;
  };

}
