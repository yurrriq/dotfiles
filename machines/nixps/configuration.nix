{ config, lib, pkgs, ... }:
let
  username = "yurrriq";
in
{
  imports = [
    <nixos-hardware/dell/xps/15-9560/intel>
    <nixos-hardware/common/pc/laptop/ssd>
    ./hardware-configuration.nix
    ../../nix
    ../../modules/common.nix
    ../../modules/location.nix
    ../../modules/nixos.nix
    ../../modules/packages.nix
    <home-manager/nixos>
  ];

  airportCode = "MSP";

  boot.blacklistedKernelModules = [ "nouveau" "nvidia" "psmouse" ];

  boot.initrd.luks.devices = {
    root = {
      device = "/dev/nvme0n1p2";
    };
  };

  environment.homeBinInPath = true;

  environment.pathsToLink = [
    "/lib/aspell"
    "/share/emacs/site-lisp"
    "/share/fish"
  ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/024a1168-9949-4cb2-bbd1-4b19a9d49ef2";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/7574-B246";
    fsType = "vfat";
  };

  fileSystems."/var/lib/docker/plugins" = {
    device = "/var/lib/docker/plugins";
    fsType = "none";
    options = [ "bind" ];
  };

  fileSystems."/var/lib/docker/overlay2" = {
    device = "/var/lib/docker/overlay2";
    fsType = "none";
    options = [ "bind" ];
  };

  fonts.fonts = with pkgs; [
    fira-code
    fira-code-symbols
  ];

  hardware.bluetooth = {
    enable = true;
    config = {
      General = {
        Enable = "Source,Sink,Media,Socket";
      };
    };
  };

  hardware.bumblebee.enable = false;
  hardware.nvidiaOptimus.disable = false;
  home-manager.useUserPackages = true;
  home-manager.users."${username}" = args:
    import ./home.nix (args // { inherit pkgs; });

  networking.hostName = "nixps";

  nix = {

    buildCores = 8;

    nixPath = [
      "nixos-config=/etc/nixos/configuration.nix"
      "nixpkgs-overlays=/etc/nixos/overlays"
    ];

    trustedUsers = [ "root" username ];

  };

  nixpkgs.overlays =
    let
      path = <nixpkgs-overlays>;
    in
      with builtins;
      map (n: import (path + ("/" + n)))
        (
          filter (
            n: match ".*\\.nix" n != null
            || pathExists (path + ("/" + n + "/default.nix"))
          )
            (attrNames (readDir path))
        )
      ++ (
        with (import <nurpkgs> {}).overlays; [
          nur
          engraving
          git
          node
        ]
      );

  security.sudo = {
    enable = true;
    extraConfig = ''
      ${username} ALL=(ALL) NOPASSWD: ALL
    '';
  };

  services.xserver.displayManager.lightdm.autoLogin = {
    enable = true;
    user = username;
  };

  services.xserver.dpi = 180;

  users.mutableUsers = false;
  users.users."${username}" = {
    name = username;
    hashedPassword = lib.fileContents (./. + "/secrets/${username}.hashedPassword");
    group = "users";
    extraGroups = [
      "audio"
      "disk"
      "docker"
      "http"
      "networkmanager"
      "systemd-journal"
      "video"
      "wheel"
    ];
    createHome = true;
    uid = 1000;
    home = "/home/${username}";
    shell = "/etc/profiles/per-user/${username}/bin/fish";
  };

}
