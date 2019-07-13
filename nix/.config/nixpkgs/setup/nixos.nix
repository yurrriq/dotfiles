{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    keybase-gui
    (pass.withExtensions (exts: with exts; [
      pass-tomb
      pass-otp
    ]))
  ];

  programs.browserpass.enable = true;

  services.kbfs.enable = true;

}
