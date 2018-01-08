{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./emacs.nix
    ./yubikey-gpg.nix
  ];

  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  boot.initrd.luks.devices = {
    root = {
      device = "/dev/nvme0n1p2";
    };
  };  

  environment = {
    shellAliases = {
      agn = "ag --nogroup";
      agq = "ag -Q";
      k = "clear";
      l = "ls -Glah";
      ll = "ls -Glh";
      ls = "ls -G";
    };
    
    systemPackages = with pkgs; ([
      aspell
      autojump
      google-chrome
      git
      gitAndTools.hub
      htop
      iosevka
      keybase
      psmisc
      qpdfview
      silver-searcher
      terminator
      tree
    ] ++ (with haskellPackages; [
      idris
    ]) ++ (with xorg; [
      xbacklight
    ]));
  };

  i18n = {
    consoleFont = "latarcyrheb-sun32";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  networking = {
    # firewall = {
    #   allowedTCPPorts = [ ... ];
    #   allowedUDPPorts = [ ... ];
    #   enable = false;
    # };
    hostName = "nixps";
    networkmanager.enable = true;
  };

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = super: let self = super.pkgs; in {
    };
  };

  programs = {
    bash.enableCompletion = true;

    fish = {
      enable = true;
      shellInit = ''
        source ${pkgs.autojump}/share/autojump/autojump.fish
      '';
    };

    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };

    # TODO
    # tmux.enable = true;
  };

  security = {
    pam.enableU2F = true;
  };
  
  services = {
    # openssh.enable = true;

    # printing.enable = true;

    # https://raw.githubusercontent.com/Yubico/libu2f-host/af4812c/70-u2f.rules
    udev.extraRules = pkgs.stdenv.lib.strings.fileContents ./70-u2f.rules;
    
    xserver = {
      autorun = true;

      desktopManager = {
        gnome3.enable = true;
	xterm.enable = false;
	default = "none";
      };
      
      displayManager = {
        lightdm.enable = true;
      };

      enable = true;

      inputClassSections = [
        ''
          Identifier "touchpad"
	  Driver "libinput"
	  MatchIsTouchpad "on"	 
	  Option "AccelSpeed" "1.0"
	''
      ];

      layout = "us";

      libinput = {
        enable = true;
	naturalScrolling = false;
	tapping = true;
	disableWhileTyping = true;
      };

      monitorSection = ''
        DisplaySize 406 228
      '';

      multitouch = {
        enable = true;
        invertScroll = true;
	ignorePalm = true;
      };
      
      # FIXME
      # videoDrivers = [
      #   "displaylink"
      #   "modesetting"
      # ];

      windowManager = {
        i3.enable = true;
      };

      xkbOptions = ''
        ctrl:nocaps
      '';
    };
  };

  system.stateVersion = "17.09";

  time.timeZone = "America/Chicago";

  users.extraUsers.yurrriq = {
    name = "yurrriq";
    group = "users";
    extraGroups = [
      "wheel" "disk" "audio" "video"
      "networkmanager" "systemd-journal"
    ];
    createHome = true;
    uid = 1000;
    home = "/home/yurrriq";
    shell = "/run/current-system/sw/bin/fish";
  };
}
