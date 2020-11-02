{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    gitAndTools.pass-git-helper
  ];

  programs.password-store = {
    enable = true;
    package = pkgs.pass.withExtensions (exts: with exts; [
      pass-genphrase
      pass-otp
      pass-tomb
      pass-update
    ]);
    settings = {
      PASSWORD_STORE_DIR = "${config.home.homeDirectory}/.password-store";
    };
  };

}
