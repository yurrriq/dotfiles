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
    <setup/location/msp.nix>
    <setup/nixos.nix>
    <setup/packages.nix>
    "${home-manager}/nixos"
  ];

  environment = {
    pathsToLink = [
      "/lib/aspell"
      "/share/emacs/site-lisp"
    ];
    systemPackages = with pkgs; ([
      aws-iam-authenticator
      ec2instanceconnectcli
      # dhall
      # dhall-json
      # docker-compose
      ghc
      jdk
      naal
      networkmanager-openconnect
      # next
      openconnect
      pavucontrol
      renderizer
    ] ++ (with nodePackages; [
      aws-azure-login
    ]));
  };

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
      "nixpkgs=${nixpkgs}"
      "nixpkgs-overlays=/home/${username}/.config/nixpkgs/overlays"
      "nur=${nur}"
      "nurpkgs=/home/${username}/.config/nurpkgs"
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

  services.blueman.enable = true;

  # TODO: services.lorri.enable = true;

  services.xserver.monitorSection = ''
    DisplaySize 406 228
  '';

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
    shell = "/etc/profiles/per-user/${username}/bin/fish";
  };

  virtualisation.docker.enable = true;
}
