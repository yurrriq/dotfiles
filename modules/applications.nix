{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    # TODO: clementine
    google-chrome
    libreoffice
    spotify
    terminator
    thunderbird
  ];

}
