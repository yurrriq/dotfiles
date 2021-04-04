{ pkgs, ... }:
{
  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      (nerdfonts.override { fonts = [ "Iosevka" ]; })
    ];
  };
  programs.bash.enableCompletion = true;
}
