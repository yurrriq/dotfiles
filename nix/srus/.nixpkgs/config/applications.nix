{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    clementine
    # copyq
    jdiskreport
    # FIXME: kdiff3
    skim
  ];

  nixpkgs.config.packageOverrides = super: {
    clementine = super.callPackage ../pkgs/applications/audio/clementine {};
    copyq = super.callPackage ../pkgs/applications/misc/copyq {};
    skim = super.callPackage ../pkgs/applications/misc/skim {};
  };

}
