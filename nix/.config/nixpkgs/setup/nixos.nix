{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    keybase-gui
    (pass.withExtensions (ext: [ ext.pass-tomb ]))
  ];

  programs.browserpass.enable = true;

  services = {
    kbfs.enable = true;
    keybase.enable = true;
  };
}
