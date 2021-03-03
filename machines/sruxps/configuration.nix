{ config, lib, pkgs, ... }:
let
  username = "e.bailey";
in
{
  airportCode = "MSP";
  boot.initrd.luks.devices = {
    cryptkey.device = "/dev/disk/by-uuid/ed7a10f2-d674-41e0-9a90-0b55b55459d7";

    cryptroot = {
      device = "/dev/disk/by-uuid/638d6b94-a52e-4840-ac1b-a42877a4fc23";
      keyFile = "/dev/mapper/cryptkey";
    };

    cryptswap = {
      device = "/dev/disk/by-uuid/e9787457-a322-470a-836e-266831ea1405";
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
  environment.homeBinInPath = true;
  environment.pathsToLink = [
    "/lib/aspell"
    "/share/emacs/site-lisp"
    "/share/fish"
    # FIXME: "/share/icons"
  ];
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/0d1dbc68-7db1-4dc4-ab42-743a82b1caa2";
    fsType = "ext4";
    options = [ "noatime" "nodiratime" "discard" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/EEAB-CEBD";
    fsType = "vfat";
  };

  fileSystems."/mnt/music" = {
    device = "192.168.1.147:/volume1/homes/eric/music";
    fsType = "nfs";
    options = [ "noatime" "noauto" "rw" "x-systemd.automount" ];
  };
  hardware.bluetooth = {
    enable = true;
    config = {
      General = {
        Enable = "Source,Sink,Media,Socket";
      };
    };
  };
  hardware.opengl.enable = true;
  home-manager.useUserPackages = true;
  home-manager.users."${username}" = args:
    import ./home.nix (args // { inherit pkgs; });
  networking.hostName = "MSP-EBAILEY01";

  networking.interfaces.wlp1s0.useDHCP = true;
  networking.useDHCP = false; # NOTE: Deprecated, so set it false.
  nix.trustedUsers = [ "root" username ];
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

  # services.xserver.videoDrivers = [ "modesetting" ];
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
  virtualisation.docker.enable = true;
}
