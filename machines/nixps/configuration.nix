{ config, lib, pkgs, ... }:
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
    config = {
      General = {
        Enable = "Source,Sink,Media,Socket";
      };
    };
  };
  hardware.pulseaudio.support32Bit = true;
  hardware.opengl.driSupport32Bit = true;
  home-manager.useUserPackages = true;
  home-manager.users."${username}" = args:
    import ./home.nix (args // { inherit pkgs; });
  networking.firewall = {
    enable = true;
    allowedTCPPortRanges = [
      # { from = 8000; to = 8000; }
    ];
  };

  networking.hostName = "nixps";

  nix.trustedUsers = [ "root" username ];

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
    dpi = 180;
  };

  # FIXME
  # sops = {
  #   gnupgHome = "/home/yurrriq/.gnupg";
  #   secrets."${username}.hashedPassword" = {
  #     format = "binary";
  #     sopsFile = ./. + "/secrets/${username}.hashedPassword.enc";
  #   };
  #   sshKeyPaths = [];
  # };

  users.mutableUsers = false;
  users.users."${username}" = {
    name = username;
    hashedPassword = lib.fileContents (./. + "/secrets/${username}.hashedPassword");
    # FIXME: hashedPassword = lib.fileContents config.sops.secrets."${username}.hashedPassword".path;
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
