{ config, lib, pkgs, ... }:

{

  imports = [
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
  ];

  boot.initrd.availableKernelModules = [
    "nvme"
    "rtsx_pci_sdmmc"
    "sd_mod"
    "usb_storage"
    "xhci_pci"
  ];

  boot.kernelModules = [ "kvm-intel" ];

  boot.initrd.luks.devices = {
    cryptkey = {
      device = "/dev/disk/by-uuid/603b64c6-8544-4b43-9b6a-7d8a08091514";
    };

    cryptroot = {
      device = "/dev/disk/by-uuid/a81783fe-31ec-4762-a845-4b5be1900e61";
      keyFile = "/dev/mapper/cryptkey";
    };

    cryptswap = {
      device = "/dev/disk/by-uuid/565c0358-110e-4279-ba59-619cb2cc1ebf";
      keyFile = "/dev/mapper/cryptkey";
    };
  };

  fileSystems = {
    "/boot" = {
      device = "/dev/disk/by-uuid/3898-907E";
      fsType = "vfat";
    };

    "/" = {
      device = "/dev/disk/by-uuid/3e6e5ef8-7c9d-4759-94bd-44ac093add8a";
      fsType = "ext4";
    };
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/cae2164d-72b5-4f60-b369-e845606d03be"; }
  ];

  nix.maxJobs = lib.mkDefault 8;

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

}
