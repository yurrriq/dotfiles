{ config, pkgs, ... }:

let
  _nixpkgs =
    let
      inherit (builtins) fetchTarball fromJSON readFile;
      inherit (fromJSON (readFile ./nixpkgs-src.json)) owner repo rev sha256;
    in
    fetchTarball {
      url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
      inherit sha256;
    };
    # "$HOME/src/github.com/NixOS/nixpkgs";
    # "$HOME/.nix-defexpr/channels/nixpkgs";
in

{
  imports = [
    ./hardware-configuration.nix
  ] ++ [
    ./modules/applications.nix
    ./modules/beam.nix
    ./modules/coq.nix
    ./modules/emacs.nix
    ./modules/git.nix
    ./modules/idris.nix
    ./modules/lilypond.nix
    ./modules/pass.nix
    ./modules/shell.nix
    ./modules/tomb.nix
    ./modules/yubikey-gpg.nix
  ] ++ [
    ./modules/clients/invisiblefriend.nix
    ./modules/clients/yellowdig.nix
    # ./modules/clients/voicehive.nix
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

  nix.nixPath = [
    "nixpkgs=${_nixpkgs}"
    "nixos-config=/etc/nixos/configuration.nix"
  ];

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = super: let self = super.pkgs; in {
      browserpass = super.callPackage ./pkgs/tools/security/browserpass {};
      docker = super.docker-edge;
      # iosevka = super.iosevka.override {
      #   design = [ "ligset-idris" ];
      #   set = "idris";
      # };
      # noweb = super.callPackage ./pkgs/development/tools/literate-programming/noweb {};
    };
  };

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
      "docker" "http"
    ];
    createHome = true;
    uid = 1000;
    home = "/home/yurrriq";
    shell = "/run/current-system/sw/bin/fish";
  };

  virtualisation.docker.enable = true;
}
