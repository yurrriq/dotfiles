{ config, pkgs, ... }:

with import ./srcs { local = false; };

{
  imports = [
    ./hardware-configuration.nix
  ] ++ [
    ./modules/tomb.nix
    ./modules/yubikey-gpg.nix
  ] ++ [
    ./config/applications.nix
    ./config/beam.nix
    ./config/docker.nix
    ./config/emacs.nix
    ./config/engraving.nix
    ./config/git.nix
    ./config/pass.nix
    ./config/shell.nix
    ./config/theorem-proving.nix
  ] ++ [
    ./config/clients/invisiblefriend.nix
    ./config/clients/yellowdig.nix
    # ./config/clients/voicehive.nix
  ];

  boot.kernelPackages = pkgs.linuxPackages_4_9;

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
      nix
      tellico
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
    hostName = "nixps";
    networkmanager.enable = true;
  };

  nix = {

    binaryCaches = [
      "https://cache.nixos.org"
      "https://yurrriq.cachix.org"
      "https://yurrriq-nur-packages.cachix.org"
    ];

    binaryCachePublicKeys = [
      # "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "yurrriq.cachix.org-1:evpJ5wKluf7QOCcv69VkIxCOtHgubrqXlZpp3JAXLBE="
      "yurrriq-nur-packages.cachix.org-1:7kbjuGBUZcWf876g2cdelmIQXrXzOhpMVBqYOyyAv70="
    ];

    buildCores = 8;

    gc.automatic = true;

    nixPath = [
      "nixos-config=/etc/nixos/configuration.nix"
      "nixpkgs=${_nixpkgs}"
      # FIXME: "nixpkgs-overlays=/etc/nixos/overlays-compat/"
    ];

    trustedUsers = [ "root" "yurrriq" ];

  };

  nixpkgs.config.allowUnfree = true;

  nixpkgs.overlays = with nur-no-pkgs.repos.yurrriq.overlays; [
    nur
    engraving
    git
    node
  ];

  programs.tomb = {
    enable = true;
    resize = true;
    slam = true;
  };

  security.sudo.extraConfig = ''
    yurrriq ALL=(ALL) NOPASSWD: ALL
  '';

  services = {
    # openssh.enable = true;
    # printing.enable = true;

    redshift = {
      enable = true;
      latitude = "44.93";
      longitude = "-93.24";
      temperature.night = 2300;
    };

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

      # monitorSection = ''
      #   DisplaySize 406 228
      # '';

      multitouch = {
        enable = true;
        invertScroll = true;
        ignorePalm = true;
      };

      # screenSection = ''
      #   Option "RandRRotation" "on"
      # '';

      # FIXME
      # videoDrivers = [
      #   "displaylink"
      #   "modesetting"
      # ];

      windowManager = {
        i3.enable = true;
      };

      xkbOptions = "ctrl:nocaps";

      xrandrHeads = [
        # "HDMI1"
        {
          output = "eDP1";
          primary = true;
          monitorConfig = ''
            DisplaySize 406 228
          '';
        }
      ];
      resolutions = [
        { x = "3840"; y = "2160"; }
        # { x = "1080"; y = "1920"; }
      ];
    };
  };

  system.stateVersion = "18.03";

  time.timeZone = "America/Chicago";

  users.extraUsers.yurrriq = {
    name = "yurrriq";
    group = "users";
    extraGroups = [
      "wheel" "disk" "audio" "video"
      "networkmanager" "systemd-journal"
      "http"
    ];
    createHome = true;
    uid = 1000;
    home = "/home/yurrriq";
    shell = "/run/current-system/sw/bin/fish";
  };

  yubikey-gpg.enable = true;
}
