{ pkgs, ... }:

{

  imports = (with (import <nur> {}).repos.yurrriq.modules; [
    pass
    tomb
  ]);

  environment = {
    shellAliases = rec {
      # Old Darwin habits
      pbcopy = "xclip -sel clipboard";
      pbpaste = "${pbcopy} -o";
    };
    systemPackages = [
      # NOTE: https://blorg.ericb.me/2019/10/browserpass-on-nixos/
      pkgs.browserpass
    ];
  };

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      iosevka
    ];
  };

  i18n = {
    consoleFont = "latarcyrheb-sun32";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  programs = {
    browserpass.enable = true;
    pass = {
      enable = true;
      git-helper = true;
      otp = true;
    };
    tomb = {
      enable = true;
      resize = true;
      slam = true;
    };
  };

  services.kbfs.enable = true;

}
