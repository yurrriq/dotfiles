{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    keybase-gui
    (pass.withExtensions (exts: with exts; [
      pass-tomb
      pass-otp
    ]))
  ];

  programs.browserpass.enable = false;

  services.kbfs.enable = true;

}
