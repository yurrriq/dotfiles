{ config, lib, pkgs, ... }:
let
  username = "e.bailey";
in
{
  airportCode = "MSP";
  boot.initrd.luks.devices = {
    cryptkey.device = "/dev/disk/by-uuid/2a44a760-206c-448d-a126-527b8b63f5d0";

    cryptroot = {
      device = "/dev/disk/by-uuid/6cd51820-547b-4378-b566-47f8cdbc46df";
      keyFile = "/dev/mapper/cryptkey";
    };

    cryptswap = {
      device = "/dev/disk/by-uuid/7d80e701-3a6b-4bb0-b8a3-dd5dfb432cdd";
      keyFile = "/dev/mapper/cryptkey";
    };
  };
  boot.kernel.sysctl = {
    "vm.swappiness" = 1;
  };
  boot.kernelModules = [
    "coretemp"
    "i915.enable_psr=0"
  ];
  boot.kernelParams = [ "acpi_rev_override" "mem_sleep_default=deep" "intel_iommu=igfx_off" ];
  environment.homeBinInPath = true;
  environment.pathsToLink = [
    "/lib/aspell"
    "/share/emacs/site-lisp"
    "/share/fish"
    # FIXME: "/share/icons"
  ];
  environment.systemPackages = with pkgs; [
    fish-kubectl-completions
    kubelogin
  ];
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/75e8b8ef-8143-4f93-a60b-c5d53adb80d3";
    fsType = "ext4";
    options = [ "noatime" "nodiratime" "discard" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/DF46-7ADE";
    fsType = "vfat";
  };
  hardware.bluetooth = {
    enable = true;
    settings = {
      General = {
        Enable = "Source,Sink,Media,Socket";
      };
    };
  };
  hardware.acpilight.enable = true;
  hardware.opengl.enable = true;
  home-manager.users."${username}" = import ./home.nix;
  networking.hostName = "MSP-EBAILEY01";
  nix = {
    binaryCaches = [
      "https://sportradar.cachix.org"
    ];
    binaryCachePublicKeys = [
      "sportradar.cachix.org-1:6MyCzOfUMeMTxU5QnogkyYOBtr5f5atW/qeS+TjmUfE="
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

  services.thermald.enable = lib.mkForce false; # FIXME

  services.tlp.enable = true;

  services.xserver = {
    config = ''
      Section "Device"
        Identifier  "Intel Graphics"
        Driver      "intel"
        Option      "TearFree"        "true"
        Option      "SwapbuffersWait" "true"
        BusID       "PCI:0:2:0"
      EndSection
    '';

    displayManager.autoLogin = {
      enable = true;
      user = username;
    };

    monitorSection = ''
      DisplaySize 406 228
    '';

    screenSection = ''
      Option         "AllowIndirectGLXProtocol" "off"
      Option         "TripleBuffer" "on"
    '';
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
    uid = 1001;
    shell = "/etc/profiles/per-user/${username}/bin/fish";
  };
}
