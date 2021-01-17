{ pkgs, ... }:

{
  boot = {
    cleanTmpDir = true;
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };
  console.font = "Lat2-Terminus16";
  console.keyMap = "us";
  i18n.defaultLocale = "en_US.UTF-8";
  location.provider = "manual";
  networking.networkmanager.enable = true;
  nixpkgs.config.allowUnfree = true;
  services = {
    fstrim.enable = true;

    logind.lidSwitch = "hibernate";

    kbfs.enable = true;

    redshift = {
      enable = true;
      temperature.night = 2300;
    };

    unclutter = {
      enable = false; # FIXME
      extraOptions = [ "exclude-root" "ignore-scrolling" ];
      threshold = 1;
      timeout = 1;
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

      layout = "us";

      libinput = {
        enable = true;
        accelSpeed = "1.0";
        disableWhileTyping = true;
        naturalScrolling = false;
        tapping = true;
      };

      videoDrivers = [
        "intel"
      ];

      xkbOptions = "ctrl:nocaps,compose:ralt";
    };
  };
  hardware.pulseaudio = {
    enable = true;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    package = pkgs.pulseaudioFull;
  };
  sound.enable = true;
  system.stateVersion = "20.09";
  virtualisation = {
    docker.enable = false;
    podman = {
      enable = true;
      dockerCompat = true;
    };
  };
}
