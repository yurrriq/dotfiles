{ config, lib, pkgs, ... }:

with import <setup/srcs> { local = false; };

let

  inherit (nur-no-pkgs.repos.yurrriq.lib) pinnedNixpkgs;

  username = "yurrriq";

in

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      <setup/common.nix>
      <setup/nixos.nix>
      <setup/packages.nix>
    ] ++ (with (import <nur> {}).repos.yurrriq.modules; [
    ]);

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      iosevka
    ];
   };

  nix = {
    nixPath = [
      "nixos-config=/etc/nixos/configuration.nix"
      "nixpkgs=${_nixpkgs}"
      "nixpkgs-overlays=/home/${username}/.config/nixpkgs/overlays"
      "nur=${_nur}"
      "setup=/home/${username}/.config/nixpkgs/setup"
    ];
    trustedUsers = [ username ];
  };

  nixpkgs.config.allowUnfree = true;

  nixpkgs.overlays =
    let path = <nixpkgs-overlays>; in with builtins;
      map (n: import (path + ("/" + n)))
          (filter (n: match ".*\\.nix" n != null ||
	              pathExists (path + "/" + n + "/default.nix"))
		  (attrNames (readDir path)))
    ++ (with nur-no-pkgs.repos.yurrriq.overlays; [
      nur
      git
      node
    ]);

  # Select internationalisation properties.
  i18n = {
    consoleFont = "latarcyrheb-sun32";
    consoleKeyMap = "us";
    # defaultLocale = "en_US.UTF-8";
  };

  powerManagement.cpuFreqGovernor = "powersave";

  # Set your time zone.
  time.timeZone = "America/Chicago";

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  security.sudo = {
    enable = true;
    extraConfig = ''
      ${username} ALL=(ALL) NOPASSWD: ALL
    '';
  };

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    autorun = true;
    # Enable touchpad support.
    libinput = {
      enable = true;
      naturalScrolling = false;
      tapping = true;
      disableWhileTyping = true;
    };
    multitouch = {
      enable = true;
      invertScroll = true;
      ignorePalm = true;
    };
    desktopManager = {
      gnome3.enable = false;
      xterm.enable = false;
      default = "none";
    };
    displayManager.lightdm.enable = true;
    windowManager = {
      default = "i3";
      i3.enable = true;
    };
    # monitorSection = ''
    #   DisplaySize 406 228
    # '';
    xkbOptions = "ctrl:nocaps";
  };

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers."${username}" = {
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

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?

}
