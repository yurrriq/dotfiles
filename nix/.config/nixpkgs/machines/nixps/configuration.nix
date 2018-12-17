{ config, pkgs, ... }:

with import <setup/srcs>  { local = false; };

let

  username = "yurrriq";

in

{
  imports = let nur-no-pkgs = (import <nur> {}); in [
    ./hardware-configuration.nix
    <setup/common.nix>
    <setup/nixos.nix>
    <setup/packages.nix>
  ] ++ (with (import <nur> {}).repos.yurrriq.modules; [
    tomb
    yubikey-gpg
  ]);

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
      exercism
      libreoffice
      nix
      tellico
      xorg.xbacklight
    ];
  };

  fonts.fonts = with pkgs; [
    fira-code
    fira-code-symbols
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
      "nixpkgs-overlays=/home/${username}/.config/nixpkgs/overlays"
      "nur=${_nur}"
      "setup=/home/${username}/.config/nixpkgs/setup"
    ];

    trustedUsers = [ "root" username ];

  };

  nixpkgs.config.allowUnfree = true;

  nixpkgs.overlays =
    let path = <nixpkgs-overlays>; in with builtins;
      map (n: import (path + ("/" + n)))
          (filter (n: match ".*\\.nix" n != null ||
                      pathExists (path + ("/" + n + "/default.nix")))
                  (attrNames (readDir path)))
    ++ (with (import <nur> {}).repos.yurrriq.overlays; [
      nur
      engraving
      git
      node
    ]);

  programs.tomb = {
    enable = true;
    resize = true;
    slam = true;
  };

  security.sudo.extraConfig = ''
    ${username} ALL=(ALL) NOPASSWD: ALL
  '';

  services = {

    logind = {
      lidSwitch = "hybrid-sleep";
    };

    redshift = {
      enable = true;
      latitude = "44.93";
      longitude = "-93.24";
      temperature.night = 2300;
    };

    xserver = {
      autorun = true;

      desktopManager = {
        gnome3.enable = false;
        xterm.enable = false;
        default = "none";
      };

      displayManager = {

        lightdm = {
          autoLogin = {
            enable = true;
            user = username;
          };
          enable = true;
        };

        # TODO:
        # sessionCommands = ''
        #   ${pkgs.xorg.xrdb}/bin/xrdb -merge <<<"Xcursor.size: 64"
        # '';

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
        default = "i3";
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

  system.stateVersion = "18.09";

  time.timeZone = "America/Chicago";

  users.extraUsers."${username}" = {
    name = username;
    group = "users";
    extraGroups = [
      "wheel" "disk" "audio" "video"
      "networkmanager" "systemd-journal"
      "http" "docker"
    ];
    createHome = true;
    uid = 1000;
    home = "/home/${username}";
    shell = "/run/current-system/sw/bin/fish";
  };

  virtualisation.docker.enable = true;

  yubikey-gpg.enable = true;
}
