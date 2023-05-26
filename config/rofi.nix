{ ... }:

{
  imports = [
    ./fonts.nix
  ];

  programs.rofi = {
    enable = true;
    font = "Iosevka Nerd Font 18";
    pass = {
      enable = true;
      stores = [ "~/.password-store" ];
    };
    theme = "purple";
  };
}
