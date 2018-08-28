{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    lilypond-with-fonts
    musescore
    skim
  ];

}
