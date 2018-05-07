{ config, pkgs, ... }:

{
  environment.systemPackages = (with pkgs; [
    nodejs
  ]) ++ (with pkgs.nodePackages; [
    # aglio
    # aws-sam-local
    diff-so-fancy
    # dispatch-proxy
    # js-beautify
    json-minify
    # jsonlint
    node2nix
    # resume-cli
    # speed-test
    vmd
  ]) ++ (with pkgs.nodePackages_8_x; [
    # heroku-cli
  ]);

  nixpkgs.config.packageOverrides = super: let self = super.pkgs; in rec {
    nodejs = super.nodejs-6_x;
    nodePackages = super.nodePackages_6_x //
      super.callPackage ./pkgs/development/node-packages {
      inherit (super) pkgs;
      nodejs = super.nodejs-6_x;
    };
    nodePackages_8_x = super.nodePackages_8_x //
      super.callPackage ./pkgs/development/node-packages-8x {
      inherit (super) pkgs;
      nodejs = super.nodejs-8_x;
    };
  };
}
