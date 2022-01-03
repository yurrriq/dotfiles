{ lib, pkgs, ... }:

{
  boot = {
    cleanTmpDir = true;
    loader.systemd-boot.enable = lib.mkDefault true;
    loader.efi.canTouchEfiVariables = lib.mkDefault true;
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

      libinput = {
        enable = true;
        touchpad = {
          accelSpeed = "1.0";
          disableWhileTyping = true;
          naturalScrolling = false;
          tapping = true;
        };
      };

      videoDrivers = lib.mkDefault [ "intel" ];

      xkbOptions = "ctrl:nocaps,compose:ralt";
    };
  };
  hardware.pulseaudio = {
    enable = true;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    package = pkgs.pulseaudioFull;
  };
  sound.enable = true;
  system.stateVersion = "21.11";
}
