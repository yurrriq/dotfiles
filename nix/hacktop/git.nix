{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    diff-pdf
    git
    git-crypt
    # git-lfs
    # TODO: gitAndTools.ghi (add package)
    gitAndTools.gitflow
    gitAndTools.hub
    gnupg
  ];

  nixpkgs.config.packageOverrides = super: rec {
    diff-pdf = super.callPackage ./pkgs/tools/text/diff-pdf {
      inherit (super.darwin.apple_sdk.frameworks) Cocoa;
    };
  };

  # programs = {
  #   gnupg.agent = {
  #     enable = true;
  #     enableSSHSupport = true;
  #   };
  # };
}
