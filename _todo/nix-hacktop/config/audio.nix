{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    ffmpeg
    flac
    fluidsynth
    lame
    timidity
  ];

  nixpkgs.config.packageOverrides = super: rec {
    timidity = super.callPackage ./pkgs/tools/misc/timidity {
      inherit (super.darwin.apple_sdk.frameworks) CoreAudio;
    };
  };
}
