{ config, lib, pkgs, ... }:

let

  username = "e.bailey";

in

{

  imports = [
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
    cryptroot.keyFile = "/dev/mapper/cryptkey";
    cryptswap = {
      device = "/dev/disk/by-uuid/565c0358-110e-4279-ba59-619cb2cc1ebf";
      keyFile = "/dev/mapper/cryptkey";
    };
  };

  # nurpkgs = fetchTarball "https://github.com/yurrriq/nur-packages/tarball/6490d8f536fc7c2c1947a40aa875a9764cc867ee";
  # nurpkgs = "/home/${username}/src/github.com/yurrriq/nur-packages";

  environment.pathsToLink = [
    "/lib/aspell"
    "/share/emacs/site-lisp"
    "/share/fish"
  ];

  home-manager.useUserPackages = true;
  home-manager.users."${username}" = args:
    import ./home.nix (args // { inherit pkgs; });

  networking.hostName = "MSP-EBAILEY01";

  nix = {
    buildMachines = [
      {
        hostName = "nix-builder-0";
        sshKey = "/root/.ssh/nix-builder-0.pem";
        sshUser = "root";
        system = "x86_64-linux";
        maxJobs = 2;
      }
    ];
    distributedBuilds = true;
    extraOptions = ''
      builders-use-substitutes = true
    '';
    nixPath = [
      "nixos-config=/etc/nixos/configuration.nix"
      "nixpkgs-overlays=/etc/nixos/overlays"
    ];
    trustedUsers = [ "root" username ];
  };

  nixpkgs.overlays =
    let path = <nixpkgs-overlays>; in with builtins;
      map (n: import (path + ("/" + n)))
          (filter (n: match ".*\\.nix" n != null ||
                      pathExists (path + ("/" + n + "/default.nix")))
                  (attrNames (readDir path)))
    ++ (with (import <nurpkgs> {}).overlays; [
      nur
      git
      node
    ]);

  security.sudo = {
    enable = true;
    extraConfig = ''
      ${username} ALL=(ALL) NOPASSWD: ALL
    '';
  };

  hardware.bluetooth = {
    enable = true;
    extraConfig = ''
      [General]
      Enable=Source,Sink,Media,Socket
    '';
  };
  services.blueman.enable = true;

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
      "wheel" "disk" "audio" "video"
      "networkmanager" "systemd-journal"
      "http" "docker"
    ];
    createHome = true;
    uid = 1001;
    home = "/home/${username}";
    shell = "/etc/profiles/per-user/${username}/bin/fish";
  };

  virtualisation.docker.enable = true;
}
