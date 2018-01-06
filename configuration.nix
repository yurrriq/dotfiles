{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./yubikey-gpg.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices = {
    root = {
      device = "/dev/nvme0n1p2";
    };
  };

  networking.hostName = "nixps";
  networking.networkmanager.enable = true;

  i18n = {
    consoleFont = "latarcyrheb-sun32";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "America/Chicago";

  nixpkgs.config.allowUnfree = true;

  nixpkgs.config.packageOverrides = super: let self = super.pkgs; in {
  };

  environment.systemPackages = with pkgs; ([
    aspell
    autojump
    chromium
    git
    gitAndTools.hub
    htop
    # TODO: iosevka
    keybase
    psmisc
    silver-searcher
    terminator
  ] ++ (with haskellPackages; [
    idris
  ]) ++ (with xorg; [
    xbacklight
  ]));

  programs.bash.enableCompletion = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  programs.fish.enable = true;
  programs.fish.shellInit = ''
    source ${pkgs.autojump}/share/autojump/autojump.fish
  '';

  # TODO
  # programs.tmux.enable = true;

  environment.shellAliases.agn = "ag --nogroup";
  environment.shellAliases.agq = "ag -Q";
  environment.shellAliases.e = "ec";
  environment.shellAliases.ec = ''emacsclient -cna ""'';
  environment.shellAliases.et = ''emacsclient -cnw -a ""'';
  environment.shellAliases.k = "clear";
  environment.shellAliases.l = "ls -Glah";
  environment.shellAliases.ll = "ls -Glh";
  environment.shellAliases.ls = "ls -G";

  services.emacs.enable = true;
  services.emacs.package = import ./emacs.nix {
    inherit (pkgs) emacsWithPackages;
  };

  # services.openssh.enable = true;

  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # networking.firewall.enable = false;

  # services.printing.enable = true;

  services.xserver = {
    enable = true;
    layout = "us";
    autorun = true;
    libinput.enable = true;

    displayManager.lightdm.enable = true;
    windowManager.i3.enable = true;

    monitorSection = ''
      DisplaySize 406 228
    '';

    xkbOptions = "ctrl:nocaps";
    # FIXME
    # videoDrivers = [ "displaylink" "modesetting" ];
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
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

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "17.09"; # Did you read the comment?

}
