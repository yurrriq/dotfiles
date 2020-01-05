{ config, pkgs, ... }:

with import <setup/nix>  { local = false; };

let

  username = "yurrriq";

in

{
  imports = [
    ./hardware-configuration.nix
    ./secrets
    <setup/common.nix>
    <setup/nixos.nix>
    <setup/packages.nix>
  ] ++ (with (import <nur> {}).repos.yurrriq.modules; [
    yubikey-gpg
  ]);

  boot.initrd.luks.devices = {
    root = {
      device = "/dev/nvme0n1p2";
    };
  };

  environment = {
    systemPackages = with pkgs; [
      carla
      reaper
    ];
  };

  fonts.fonts = with pkgs; [
    fira-code
    fira-code-symbols
  ];

  location = {
    latitude = 44.93;
    longitude = -93.24;
    provider = "manual";
  };

  networking.hostName = "nixps";

  nix = {

    buildCores = 8;

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
      engraving
      git
      node
    ]);

  security.sudo = {
    enable = true;
    extraConfig = ''
      ${username} ALL=(ALL) NOPASSWD: ALL
    '';
  };

  services.xserver = {
    displayManager = {
      lightdm.autoLogin = {
        enable = true;
        user = username;
      };

      # TODO
      # sessionCommands = ''
      #   ${pkgs.xorg.xrdb}/bin/xrdb -merge <<<"Xcursor.size: 64"
      # '';
    };

    dpi = 180;

    # resolutions = [
    #   # { x = "1080"; y = "1920"; }
    #   { x = "3840"; y = "2160"; }
    # ];

    # xrandrHeads = [
    #   # "HDMI1"
    #   {
    #     output = "eDP1";
    #     primary = true;
    #     monitorConfig = ''
    #       DisplaySize 508 285
    #     '';
    #   }
    # ];
  };

  time.timeZone = "America/Chicago";

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
    uid = 1000;
    home = "/home/${username}";
    shell = "/run/current-system/sw/bin/fish";
  };

  # TODO: virtualisation.docker.enable = true;

  yubikey-gpg.enable = true;
}
