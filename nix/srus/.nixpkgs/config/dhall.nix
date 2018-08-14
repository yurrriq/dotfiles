{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    dhall
    dhall-json
  ];

}
