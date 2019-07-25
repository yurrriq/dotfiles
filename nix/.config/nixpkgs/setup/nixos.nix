{ pkgs, ... }:

{

  imports = (with (import <nur> {}).repos.yurrriq.modules; [
    pass
    tomb
  ]);

  environment.systemPackages = with pkgs; [
    # keybase-gui
    (signal-desktop.override {
      spellcheckerLanguage = "en_US";
    })
    xclip
    xorg.xbacklight
  ];

  programs = {
    browserpass.enable = true;
    pass = {
      enable = true;
      git-helper = true;
      otp = true;
    };
    tomb.enable = true;
  };

  services.kbfs.enable = true;

}
