{ config, lib, pkgs, ... }:
let
  username = "yurrriq";
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
  ] ++ (
    with (import <nurpkgs> {}).modules; [
      # yubikey-gpg
    ]
  );

  airportCode = "MSP";

  boot.initrd.luks.devices = {
    root = {
      device = "/dev/nvme0n1p2";
    };
  };

  environment.pathsToLink = [
    "/lib/aspell"
    "/share/emacs/site-lisp"
    "/share/fish"
  ];

  fonts.fonts = with pkgs; [
    fira-code
    fira-code-symbols
  ];

  home-manager.useUserPackages = true;
  home-manager.users."${username}" = args:
    import ./home.nix (args // { inherit pkgs; });

  networking.hostName = "nixps";

  nix = {

    buildCores = 8;

    nixPath = [
      "nixos-config=/etc/nixos/configuration.nix"
      "nixpkgs-overlays=/etc/nixos/overlays"
    ];

    trustedUsers = [ "root" username ];

  };

  nixpkgs.overlays =
    let path = <nixpkgs-overlays>; in with builtins;
    map (n: import (path + ("/" + n)))
      (
        filter (
          n: match ".*\\.nix" n != null
          || pathExists (path + ("/" + n + "/default.nix"))
        )
          (attrNames (readDir path))
      )
    ++ (
      with (import <nurpkgs> {}).overlays; [
        nur
        engraving
        git
        node
      ]
    );

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
    uid = 1000;
    home = "/home/${username}";
    shell = "/etc/profiles/per-user/${username}/bin/fish";
  };

  virtualisation.docker.enable = true;

  # yubikey-gpg.enable = true;
}
