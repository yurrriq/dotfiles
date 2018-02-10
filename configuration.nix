{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./applications.nix
    ./emacs.nix
    ./git.nix
    # ./idris.nix
    ./shell.nix
    ./voicehive.nix
    ./yubikey-gpg.nix
  ];

  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  boot.initrd.luks.devices = {
    root = {
      device = "/dev/nvme0n1p2";
    };
  };

  environment = {
    systemPackages = with pkgs; [
      xorg.xbacklight
    ];
  };

  fonts.fonts = with pkgs; [
    iosevka
  ];

  i18n = {
    consoleFont = "latarcyrheb-sun32";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  networking = {
    # firewall = {
    #   allowedTCPPorts = [ ... ];
    #   allowedUDPPorts = [ ... ];
    #   enable = false;
    # };
    hostName = "nixps";
    networkmanager.enable = true;
  };

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = super: let self = super.pkgs; in {
      docker = super.docker-edge;
      iosevka = super.iosevka.override {
        design = [ "ligset-idris" ];
        set = "idris";
      };
      noweb = super.callPackage ./pkgs/development/tools/literate-programming/noweb {};
    };
  };

  services = {
    # nix-daemon.enable = true;
    # openssh.enable = true;
    # printing.enable = true;

    xserver = {
      autorun = true;

      desktopManager = {
        gnome3.enable = true;
        xterm.enable = false;
        default = "none";
      };

      displayManager = {
        lightdm.enable = true;
      };

      enable = true;

      inputClassSections = [
        ''
          Identifier "touchpad"
          Driver "libinput"
          MatchIsTouchpad "on"
          Option "AccelSpeed" "1.0"
        ''
      ];

      layout = "us";

      libinput = {
        enable = true;
        naturalScrolling = false;
        tapping = true;
        disableWhileTyping = true;
      };

      monitorSection = ''
        DisplaySize 406 228
      '';

      multitouch = {
        enable = true;
        invertScroll = true;
        ignorePalm = true;
      };

      # FIXME
      # videoDrivers = [
      #   "displaylink"
      #   "modesetting"
      # ];

      windowManager = {
        i3.enable = true;
      };

      xkbOptions = "ctrl:nocaps";
    };
  };

  system.stateVersion = "17.09";

  time.timeZone = "America/Chicago";

  users.extraUsers.yurrriq = {
    name = "yurrriq";
    group = "users";
    extraGroups = [
      "wheel" "disk" "audio" "video"
      "networkmanager" "systemd-journal"
      "docker" "http"
    ];
    createHome = true;
    uid = 1000;
    home = "/home/yurrriq";
    shell = "/run/current-system/sw/bin/fish";
  };

  virtualisation.docker.enable = true;
}
