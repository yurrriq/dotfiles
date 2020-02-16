{ pkgs, ... }:

{

  imports = with (import <nurpkgs> {}).modules; [
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

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      iosevka
    ];
  };

  i18n = {
    # consoleFont = "latarcyrheb-sun32";
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

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
      # FIXME
      threeshold = 1;
      timeout = 1;
    };

    xserver = {
      enable = true;

      autorun = true;

      desktopManager = {
        gnome3.enable = false;
        xterm.enable = false;
        default = "none";
      };

      displayManager.lightdm.enable = true;

      layout = "us";

      libinput = {
        enable = true;
        accelSpeed = "1.0";
        disableWhileTyping = true;
        naturalScrolling = false;
        tapping = true;
      };

      multitouch = {
        enable = true;
        ignorePalm = true;
        invertScroll = true;
      };

      videoDrivers = [
        "intel"
      ];

      windowManager = {
        default = "i3";
        i3.enable = true;
      };

      xkbOptions = "ctrl:nocaps,compose:ralt";
    };

  };

  hardware.pulseaudio = {
    enable = true;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    package = pkgs.pulseaudioFull;
  };
  sound.enable = true;


  system.stateVersion = "19.09";

  virtualisation.docker.enable = true;

}
