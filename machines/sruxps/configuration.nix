{ config, lib, pkgs, ... }:
let
  username = "e.bailey";
in
{
  airportCode = "MSP";
  boot.initrd.luks.devices = {
    cryptkey.device = "/dev/disk/by-uuid/603b64c6-8544-4b43-9b6a-7d8a08091514";

    cryptroot = {
      device = "/dev/disk/by-uuid/c4e26a93-c0c9-4680-bb79-8c0ca47df96c";
      keyFile = "/dev/mapper/cryptkey";
    };

    cryptswap = {
      device = "/dev/disk/by-uuid/565c0358-110e-4279-ba59-619cb2cc1ebf";
      keyFile = "/dev/mapper/cryptkey";
    };
  };
  boot.kernel.sysctl = {
    "vm.swappiness" = 1;
  };
  boot.kernelModules = [
    "coretemp"
  ];
  environment.homeBinInPath = true;
  environment.pathsToLink = [
    "/lib/aspell"
    "/share/emacs/site-lisp"
    "/share/fish"
    # FIXME: "/share/icons"
  ];
  environment.systemPackages = with pkgs; [
    kubelogin
  ];
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/71cfcb8a-13a9-49f0-b034-a9c8841be07b";
    fsType = "btrfs";
    options = [ "subvol=@" "rw" "noatime" "compress=zstd" "ssd" "space_cache" ];
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/71cfcb8a-13a9-49f0-b034-a9c8841be07b";
    fsType = "btrfs";
    options = [ "subvol=@home" "rw" "noatime" "compress=zstd" "ssd" "space_cache" ];
  };

  fileSystems."/nix" = {
    device = "/dev/disk/by-uuid/71cfcb8a-13a9-49f0-b034-a9c8841be07b";
    fsType = "btrfs";
    options = [ "subvol=@nix" "rw" "noatime" "compress=zstd" "ssd" "space_cache" ];
  };

  fileSystems."/var" = {
    device = "/dev/disk/by-uuid/71cfcb8a-13a9-49f0-b034-a9c8841be07b";
    fsType = "btrfs";
    options = [ "subvol=@var" "rw" "noatime" "compress=zstd" "ssd" "space_cache" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/54D0-C859";
    fsType = "vfat";
  };

  hardware.bluetooth = {
    enable = true;
    config = {
      General = {
        Enable = "Source,Sink,Media,Socket";
      };
    };
  };
  home-manager.users."${username}" = import ./home.nix;
  networking.hostName = "MSP-EBAILEY01";

  networking.interfaces.wlp1s0.useDHCP = true;
  networking.useDHCP = false; # NOTE: Deprecated, so set it false.
  nix = {
    binaryCaches = [
      # TODO: "https://sportradar.cachix.org"
    ];
    binaryCachePublicKeys = [
      # TODO: "sportradar.cachix.org-1:TODO"
    ];
    trustedUsers = [ "root" username ];
  };
  security.sudo = {
    enable = true;
    extraConfig = ''
      ${username} ALL=(ALL) NOPASSWD: ALL
    '';
  };
  services.blueman.enable = true;

  services.fwupd.enable = true;

  services.lorri.enable = false;

  services.tlp.enable = true;

  services.xserver.displayManager.autoLogin = {
    enable = true;
    user = username;
  };

  services.xserver.monitorSection = ''
    DisplaySize 406 228
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
    uid = 1001;
    shell = "/etc/profiles/per-user/${username}/bin/fish";
  };
}
