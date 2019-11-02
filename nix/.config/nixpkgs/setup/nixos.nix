{ pkgs, ... }:

{

  imports = (with (import <nur> {}).repos.yurrriq.modules; [
    pass
    tomb
  ]);

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
