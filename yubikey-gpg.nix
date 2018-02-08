{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    gnupg
    yubikey-personalization
  ];

  security = {
    pam.enableU2F = true;
  };
  
  services = {
    pcscd.enable = true;
    # https://raw.githubusercontent.com/Yubico/libu2f-host/af4812c/70-u2f.rules
    udev.extraRules = pkgs.stdenv.lib.strings.fileContents ./70-u2f.rules;
    udev.packages = with pkgs; [
      yubikey-personalization
    ];
  };
}
