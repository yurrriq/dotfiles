{ pkgs, ... }:

{

  imports = with (import <nurpkgs> { }).modules; [
    pass
    tomb
  ];

  boot = {
    cleanTmpDir = true;
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  # console.font = "latarcyrheb-sun32";
  console.font = "Lat2-Terminus16";
  console.keyMap = "us";
  i18n.defaultLocale = "en_US.UTF-8";

  location.provider = "manual";

  networking.networkmanager.enable = true;

  nixpkgs.config.allowUnfree = true;

  programs = {
    pass = {
      enable = true;
      genphrase = true;
      git-helper = true;
      otp = true;
      update = true;
    };
    tomb = {
      enable = true;
      resize = true;
      slam = true;
    };
  };

  services = {
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

      desktopManager = {
        gnome3.enable = false;
        xterm.enable = false;
      };

      displayManager.defaultSession = "none+i3";
      displayManager.lightdm.enable = true;

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

      windowManager.i3.enable = true;

      xkbOptions = "ctrl:nocaps,compose:ralt";
    };
  };

  hardware.pulseaudio = {
    enable = true;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    package = pkgs.pulseaudioFull;
  };
  sound.enable = true;

  system.stateVersion = "20.03";

  virtualisation.docker.enable = true;

}
