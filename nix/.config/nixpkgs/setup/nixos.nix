{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    (pass.withExtensions (ext: [ ext.pass-tomb ]))
  ];

  programs.browserpass.enable = true;

  services.keybase.enable = true;
}
