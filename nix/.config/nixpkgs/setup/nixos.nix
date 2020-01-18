{ pkgs, ... }:

{

  imports = (with (import <nur> {}).repos.yurrriq.modules; [
    pass
    tomb
  ]);

  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  environment = {
    shellAliases = rec {
      # Old Darwin habits
      pbcopy = "xclip -sel clipboard";
      pbpaste = "${pbcopy} -o";
    };
    systemPackages = [
      # NOTE: https://blorg.ericb.me/2019/10/browserpass-on-nixos/
      pkgs.browserpass
    ];
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

  networking = {
    networkmanager.enable = true;
  };

  nixpkgs.config.allowUnfree = true;

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

  services = {

    logind.lidSwitch = "hibernate";

    kbfs.enable = true;

    redshift = {
      enable = true;
      temperature.night = 2300;
    };

    unclutter = {
      enable = true;
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

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  system.stateVersion = "19.09";

}
