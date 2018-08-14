{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    clementine
    # copyq
    jdiskreport
    # FIXME: kdiff3
    onyx
    skim
  ];

  nixpkgs.config.packageOverrides = super: {
    clementine = super.callPackage ../pkgs/applications/audio/clementine {};
    copyq = super.callPackage ../pkgs/applications/misc/copyq {};
    onyx = super.callPackage ../pkgs/os-specific/darwin/onyx {};
    skim = super.callPackage ../pkgs/applications/misc/skim {};
  };

}
