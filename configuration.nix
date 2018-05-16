{ config, pkgs, ... }:

let
  _nixpkgs = {
    owner = "NixOS";
    repo = "nixpkgs";
    # repo = "nixpkgs-channels";
    rev = "18.03";
    # rev = "9b25b9347d97caf55732213dbf45653f866f8114";
    # rev = "1728f8e113e8038a2c1cff8c8361920d07bffebd";
    # rev = "62ccc2324f6bface7b336da3554051786efc8815";
    # rev = "b6a8398e2cf4f501bddcc42c7fba498366f16885";
    # rev = "6db7f92cc2af827e8b8b181bf5ed828a1d0f141d";
    # rev = "ef74cafd3e5914fdadd08bf20303328d72d65d6c";
  };
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
    ./modules/clients/voicehive.nix
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
    # firewall = {
    #   allowedTCPPorts = [ ... ];
    #   allowedUDPPorts = [ ... ];
    #   enable = false;
    # };
    hostName = "nixps";
    networkmanager.enable = true;
  };

  nix.nixPath = [
    # "nixpkgs=/home/yurrriq/src/github.com/NixOS/nixpkgs"
    "nixpkgs=https://github.com/${_nixpkgs.owner}/${_nixpkgs.repo}/archive/${_nixpkgs.rev}.tar.gz"
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
