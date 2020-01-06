{ config, lib, pkgs, ... }:

with import <setup/nix> { local = false; };

let

  username = "e.bailey";

in

{
  imports = [
    ./hardware-configuration.nix
    ./secrets
    <setup/common.nix>
    <setup/nixos.nix>
    <setup/packages.nix>
  ];

  environment = {
    pathsToLink = [
      "/lib/aspell"
      "/share/emacs/site-lisp"
    ];
    systemPackages = with pkgs; ([
      aws-iam-authenticator
      # dhall
      # dhall-json
      # docker-compose
      ghc
      jdk
      networkmanager-openconnect
      # next
      openconnect
      renderizer
    ]) ++ (with nodePackages; [
      aws-azure-login
    ]) ++ (with python35Packages; [
      bugwarrior
    ]));
  };

  location = {
    # NOTE: MSP
    # latitude = 44.93;
    # longitude = -93.24;
    # NOTE: ATL
    latitude = 33.76;
    longitude = -84.3;
    # NOTE: LJU
    # latitude = 46.09;
    # longitude = 14.55;
    provider = "manual";
    # provider = "geoclue2";
  };

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
      "nixpkgs=${_nixpkgs}"
      "nixpkgs-overlays=/home/${username}/.config/nixpkgs/overlays"
      "nur=${_nur}"
      "setup=/home/${username}/.config/nixpkgs/setup"
    ];
    trustedUsers = [ "root" username ];
  };

  nixpkgs.overlays =
    let path = <nixpkgs-overlays>; in with builtins;
      map (n: import (path + ("/" + n)))
          (filter (n: match ".*\\.nix" n != null ||
                      pathExists (path + ("/" + n + "/default.nix")))
                  (attrNames (readDir path)))
    ++ (with nur-no-pkgs.repos.yurrriq.overlays; [
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

  services.xserver.monitorSection = ''
    DisplaySize 406 228
  '';

  # time.timeZone = "America/Chicago";
  time.timeZone = "America/New_York";
  # time.timeZone = "Europe/Ljubljana";
  # time.timeZone = "Europe/London";
  # time.timeZone = "Europe/Oslo";

  users.mutableUsers = false;
  users.users."${username}" = {
    name = username;
    group = "users";
    extraGroups = [
      "wheel" "disk" "audio" "video"
      "networkmanager" "systemd-journal"
      "http" "docker"
    ];
    createHome = true;
    uid = 1001;
    home = "/home/${username}";
    shell = "/run/current-system/sw/bin/fish";
  };

  virtualisation.docker.enable = true;
}
