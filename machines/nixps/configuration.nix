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
  services.blueman.enable = false;
  hardware.bluetooth = {
    enable = false;
    # settings = {
    #   General = {
    #     Enable = "Source,Sink,Media,Socket";
    #   };
    # };
  };
  hardware.pulseaudio.support32Bit = true;
  hardware.opengl.driSupport32Bit = true;
  home-manager.users."${username}" = import ./home.nix;

  # musnix.enable = true;

  networking.firewall = {
    enable = true;
    allowedTCPPortRanges = [
      # { from = 8000; to = 8000; }
    ];
  };

  networking.hostName = "nixps";
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
  services.xserver = {
    displayManager.autoLogin = {
      enable = true;
      user = username;
    };
    monitorSection = ''
      DisplaySize 508 285
    '';
    dpi = 220;
  };
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
  environment.systemPackages = with pkgs; [
    xorg.xbacklight
  ];
}
