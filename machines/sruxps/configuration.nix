{ config, lib, pkgs, ... }:
let
  username = "e.bailey";
in
{

  imports = [
    <nixos-hardware/dell/xps/13-9380>
    ./hardware-configuration.nix
    ../../nix
    ../../modules/common.nix
    ../../modules/location.nix
    ../../modules/nixos.nix
    ../../modules/packages.nix
    <home-manager/nixos>
  ];

  airportCode = "MSP";

  boot.initrd.luks.devices = {
    cryptkey.device = "/dev/disk/by-uuid/603b64c6-8544-4b43-9b6a-7d8a08091514";
    cryptroot.device = "/dev/disk/by-uuid/a81783fe-31ec-4762-a845-4b5be1900e61";
    cryptroot.keyFile = "/dev/mapper/cryptkey";
    cryptswap = {
      device = "/dev/disk/by-uuid/565c0358-110e-4279-ba59-619cb2cc1ebf";
      keyFile = "/dev/mapper/cryptkey";
    };
  };

  environment.pathsToLink = [
    "/lib/aspell"
    "/share/emacs/site-lisp"
    "/share/fish"
  ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/3e6e5ef8-7c9d-4759-94bd-44ac093add8a";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/3898-907E";
    fsType = "vfat";
  };

  hardware.bluetooth = {
    enable = true;
    extraConfig = ''
      [General]
      Enable=Source,Sink,Media,Socket
    '';
  };

  home-manager.useUserPackages = true;
  home-manager.users."${username}" = args:
    import ./home.nix (args // { inherit pkgs; });

  networking.hostName = "MSP-EBAILEY01";

  nix = {
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

  services.blueman.enable = true;

  services.fwupd.enable = true;

  # TODO: services.lorri.enable = true;

  services.xserver.monitorSection = ''
    DisplaySize 406 228
  '';

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
    uid = 1001;
    home = "/home/${username}";
    shell = "/etc/profiles/per-user/${username}/bin/fish";
  };

}
