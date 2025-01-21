{ lib, pkgs, ... }:
let
  username = "yurrriq";
in
{
  airportCode = "MSP";
  boot.blacklistedKernelModules = [ "nouveau" "nvidia" "psmouse" ];
  boot.initrd.luks.devices.root.device = "/dev/nvme0n1p2";
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
  hardware.bumblebee.enable = false;
  hardware.nvidiaOptimus.disable = false;
  services.blueman.enable = true;
  hardware.bluetooth = {
    enable = true;
    settings = {
      General = {
        Enable = "Source,Sink,Media,Socket";
      };
    };
  };
  services.fwupd.enable = true;
  hardware.graphics.enable32Bit = true;
  home-manager.users."${username}" = import ./home.nix;
  networking = {
    firewall = {
      enable = true;
      allowedTCPPortRanges = [
        # { from = 8000; to = 8000; }
      ];
    };
    hostName = "nixps";
    networkmanager.enable = true;
  };
  nix = {
    settings = {
      cores = 8;
      trusted-users = [ "root" username ];
    };
  };
  security.sudo = {
    enable = true;
    extraConfig = ''
      ${username} ALL=(ALL) NOPASSWD: ALL
    '';
  };
  services.displayManager.autoLogin = {
    enable = true;
    user = username;
  };
  services.xserver = {
    monitorSection = ''
      DisplaySize 508 285
    '';
    dpi = 220;
    upscaleDefaultCursor = true;
  };
  programs.light.enable = true;
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="backlight", \
    RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/%k/brightness", \
    RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/%k/brightness"
  '';
  users.mutableUsers = false;
  users.users."${username}" = {
    name = username;
    hashedPassword = lib.fileContents "/etc/nixos/secrets/${username}.hashedPassword";
    isNormalUser = true;
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
    uid = 1000;
    shell = "/etc/profiles/per-user/${username}/bin/fish";
  };
  virtualisation.libvirtd.enable = true;
}
