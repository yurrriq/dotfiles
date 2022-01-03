{ pkgs, ... }:

{
  imports = [ <nixpkgs/nixos/modules/virtualisation/amazon-image.nix> ];

  ec2.hvm = true;

  environment.systemPackages = with pkgs; [
    bashInteractive
    git
    httpie
  ];

}
