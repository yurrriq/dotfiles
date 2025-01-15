{ lib, pkgs, ... }:

{
  boot = {
    loader.systemd-boot.enable = lib.mkDefault true;
    loader.efi.canTouchEfiVariables = lib.mkDefault true;
    tmp.cleanOnBoot = true;
  };
  console.font = "Lat2-Terminus16";
  console.useXkbConfig = true;
  location.provider = "manual";
  networking.networkmanager.enable = true;
  services = {
    logind.lidSwitch = "hibernate";
    kbfs.enable = true;
    redshift = {
      enable = true;
      temperature.night = 2300;
    };
    thermald.enable = lib.mkDefault true;
    libinput = {
      enable = true;
      touchpad = {
        accelSpeed = "1.0";
        disableWhileTyping = true;
        naturalScrolling = false;
        tapping = true;
      };
    };
    xserver = {
      enable = true;

      autorun = true;

      displayManager = {
        lightdm.enable = true;
        session = [
          {
            name = "home-manager";
            manage = "window";
            start = ''
              ${pkgs.runtimeShell} $HOME/.hm-xsession &
              waitPID=$!
            '';
          }
        ];
      };

      videoDrivers = lib.mkDefault [ "intel" ];

      xkb.options = "ctrl:nocaps,compose:ralt";
    };
  };
  system.stateVersion = "24.11";
}
