{ config, lib, pkgs, ... }:

with import <setup/srcs> { local = false; };

let

  username = "yurrriq";

in

{
  imports = [
    ./hardware-configuration.nix
    ./secrets
    <setup/common.nix>
    <setup/nixos.nix>
    <setup/packages.nix>
  ];

  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  environment = {
    pathsToLink = [
      "/lib/aspell"
      "/share/emacs/site-lisp"
    ];
    systemPackages = with pkgs; ([
      aws-iam-authenticator
      # dhall
      # dhall-json
      # docker-compose
      firefox
      fzf
      ghc
      gnome3.networkmanagerapplet
      jdk
      networkmanager-openconnect
      # next
      openconnect
      prettyping
      renderizer
    ] ++ (with haskellPackages; [
      cabal-install
      cabal2nix
      hpack
      stylish-haskell
    ]) ++ (with nodePackages; [
      aws-azure-login
      codeowners
    ]) ++ (with python35Packages; [
      bugwarrior
    ]));
  };

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      iosevka
    ];
  };

  i18n = {
    consoleFont = "latarcyrheb-sun32";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  location = {
    # NOTE: MSP
    latitude = 44.93;
    longitude = -93.24;
    # NOTE: LJU
    # latitude = 46.09;
    # longitude = 14.55;
    provider = "manual";
    # provider = "geoclue2";
  };

  networking = {
    hostName = "MSP-EBAILEY01";
    networkmanager.enable = true;
  };

  nix = {
    buildMachines = [
      {
        hostName = "nix-builder-0";
        sshKey = "/root/.ssh/nix-builder-0.pem";
        sshUser = "root";
        system = "x86_64-linux";
        maxJobs = 2;
      }
    ];
    distributedBuilds = true;
    extraOptions = ''
      builders-use-substitutes = true
    '';
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
      git
      node
    ]);

  security.sudo = {
    enable = true;
    extraConfig = ''
      ${username} ALL=(ALL) NOPASSWD: ALL
    '';
  };

  services = {
    logind = {
      lidSwitch = "hibernate";
    };

    redshift = {
      enable = true;
      temperature.night = 2200;
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

  time.timeZone = "America/Chicago";
  # time.timeZone = "Europe/London";
  # time.timeZone = "Europe/Oslo";

  users.mutableUsers = false;
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

  virtualisation.docker.enable = true;
}
