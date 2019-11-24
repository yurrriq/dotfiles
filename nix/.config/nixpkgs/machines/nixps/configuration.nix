{ config, pkgs, ... }:

with import <setup/srcs>  { local = false; };

let

  username = "yurrriq";

in

{
  imports = [
    ./hardware-configuration.nix
    <setup/common.nix>
    <setup/nixos.nix>
    <setup/packages.nix>
  ] ++ (with (import <nur> {}).repos.yurrriq.modules; [
    # FIXME: yubikey-gpg
  ]);

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
      # TODO: gnome3.nautilus
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

  location = {
    latitude = 44.93;
    longitude = -93.24;
    provider = "manual";
  };

  networking = {
    hostName = "nixps";
    networkmanager.enable = true;
  };

  nix = {

    buildCores = 8;

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
    ++ (with nur-no-pkgs.repos.yurrriq.overlays; [
      nur
      engraving
      git
      node
    ]);

  security.sudo.extraConfig = ''
    ${username} ALL=(ALL) NOPASSWD: ALL
  '';

  services = {

    logind = {
      lidSwitch = "hibernate";
    };

    redshift = {
      enable = true;
      temperature.night = 2300;
    };

    xserver = {
      enable = true;

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

        # TODO
        # sessionCommands = ''
        #   ${pkgs.xorg.xrdb}/bin/xrdb -merge <<<"Xcursor.size: 64"
        # '';

      };

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

      videoDrivers = [
        "intel"
      ];

      windowManager = {
        default = "i3";
        i3.enable = true;
      };

      xkbOptions = "ctrl:nocaps,compose:ralt";

      xrandrHeads = [
        # "HDMI1"
        {
          output = "eDP1";
          primary = true;
          monitorConfig = ''
            DisplaySize 508 285
          '';
        }
      ];
      resolutions = [
        # { x = "1080"; y = "1920"; }
        { x = "3840"; y = "2160"; }
      ];
    };
  };

  # TODO
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  system.stateVersion = "19.09";

  time.timeZone = "America/Chicago";

  # TODO: users.mutableUsers = false;

  users.users."${username}" = {
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

  # TODO: virtualisation.docker.enable = true;

  # FIXME: yubikey-gpg.enable = true;
}
