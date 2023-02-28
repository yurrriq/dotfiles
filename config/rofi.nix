{ pkgs, ... }:

{
  imports = [
    ./fonts.nix
  ];

  programs.rofi = {
    enable = true;
    font = "Iosevka Term 18";
    pass = {
      enable = true;
      stores = [ "~/.password-store" ];
    };
    theme = "purple";
  };
}
