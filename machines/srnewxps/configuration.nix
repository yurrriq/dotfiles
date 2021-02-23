{ config, lib, pkgs, ... }:
let
  username = "e.bailey";
in
{
  airportCode = "MSP";
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
  home-manager.useUserPackages = true;
  home-manager.users."${username}" = args:
    import ./home.nix (args // { inherit pkgs; });
  networking.hostName = "MSP-EBAILEY01";

  networking.interfaces.wlp2s0.useDHCP = true;
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
