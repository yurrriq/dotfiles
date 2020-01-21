{ config, lib, pkgs, ... }:

with import ../../modules/nix { local = false; };

let

  username = "yurrriq";

  airportCode = "MSP";

in

{
  imports = [
    ./hardware-configuration.nix
    ../../modules/common.nix
    (import ../../modules/location.nix { inherit lib airportCode; })
    ../../modules/nixos.nix
    ../../modules/packages.nix
    "${home-manager}/nixos"
  ] ++ (with (import <nur> {}).repos.yurrriq.modules; [
    yubikey-gpg
  ]);

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

  environment.systemPackages = with pkgs; [
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
      "nixpkgs=${nixpkgs}"
      "nixpkgs-overlays=/etc/nixos//overlays"
      "nur=${nur}"
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
    uid = 1000;
    home = "/home/${username}";
    shell = "/run/profiles/per-user/${username}/bin/fish";
  };

  virtualisation.docker.enable = true;

  yubikey-gpg.enable = true;
}
