{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    # keybase-gui
    (pass.withExtensions (exts: with exts; [
      pass-tomb
      pass-otp
    ]))
    gitAndTools.pass-git-helper
    (signal-desktop.override {
      spellcheckerLanguage = "en_US";
    })
    xclip
    xorg.xbacklight
  ];

  programs.browserpass.enable = true;

  services.kbfs.enable = true;

}
