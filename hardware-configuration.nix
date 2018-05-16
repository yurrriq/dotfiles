# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, ... }:

{
  imports =
    [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usbhid" "rtsx_pci_sdmmc" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/024a1168-9949-4cb2-bbd1-4b19a9d49ef2";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/7574-B246";
      fsType = "vfat";
    };

  fileSystems."/var/lib/docker/plugins" =
    { device = "/var/lib/docker/plugins";
      fsType = "none";
      options = [ "bind" ];
    };

  fileSystems."/var/lib/docker/overlay2" =
    { device = "/var/lib/docker/overlay2";
      fsType = "none";
      options = [ "bind" ];
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/86e29ab0-7bd5-470a-959d-75067973a9a3"; }
    ];

  nix.maxJobs = lib.mkDefault 8;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
