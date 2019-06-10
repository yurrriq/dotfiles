{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    keybase-gui
    # (pass.withExtensions (ext: [ ext.pass-tomb ]))
  ];

  programs.browserpass.enable = false;

  services = {
     kbfs.enable = true;
     keybase.enable = true;
  };

}
