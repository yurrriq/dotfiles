{ pkgs, ... }:

{

  home.file.".xmonad/matrix.png".source = ./matrix.png;

  home.packages = with pkgs; [
    flameshot
    # font-awesome_4
    i3lock
    haskellPackages.xmobar
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

  services.picom.enable = true;

  xdg.configFile."xmobar/xmobarrc".source = ../xmobar/xmobarrc;

  xsession = {
    enable = true;
    pointerCursor = {
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ-AA";
      size = 36;
    };
    scriptPath = ".hm-xsession";
    windowManager.xmonad = {
      enable = true;
      extraPackages = hpkgs: [ hpkgs.xmonad-contrib ];
      config = ./xmonad.hs;
    };
  };

}
