{ config, lib, pkgs, ... }:
{
  # Enable full keyboard access, e.g. tab in dialogs
  system.defaults.NSGlobalDomain.AppleKeyboardUIMode = 3;

  # Disable press-and-hold in favor of key repeat
  system.defaults.NSGlobalDomain.ApplePressAndHoldEnabled = false;
  system.defaults.NSGlobalDomain.InitialKeyRepeat = 10;
  system.defaults.NSGlobalDomain.KeyRepeat = 1;

  system.defaults.NSGlobalDomain.NSAutomaticDashSubstitutionEnabled = false;
  system.defaults.NSGlobalDomain.NSAutomaticQuoteSubstitutionEnabled = false;
  system.defaults.NSGlobalDomain.NSAutomaticSpellingCorrectionEnabled = true;

  system.defaults.NSGlobalDomain.NSNavPanelExpandedStateForSaveMode = true;
  system.defaults.NSGlobalDomain.NSNavPanelExpandedStateForSaveMode2 = true;

  system.defaults.dock.autohide = true;
  system.defaults.dock.orientation = "right";
  system.defaults.dock.showhidden = true;
  system.defaults.dock.mru-spaces = false;

  system.defaults.finder.AppleShowAllExtensions = true;
  system.defaults.finder.QuitMenuItem = true;
  system.defaults.finder.FXEnableExtensionChangeWarning = false;

  system.defaults.trackpad.Clicking = true;

  # TODO: dig through https://github.com/mathiasbynens/dotfiles/blob/master/.macos

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = (with pkgs; [


    # Cryptography
    gnupg

    # Git
    git
    git-crypt
    git-lfs
    gitAndTools.hub

    # Graphics
    # FIXME: graphicsmagick # NOTE: graphviz error
    # FIXME: imagemagick

    # Graphing
    gnuplot
    # FIXME: graphviz # NOTE: ApplicationServices ld error

    # Haskell
    cabal-install
    ghc
    stack

    # JVM
    clojure
    leiningen
    openjdk8

    # Lisp
    sbcl

    # Nix
    nixops
    nix-repl

    # Shell
    fish

    # SML
    # FIXME: smlnj
    # NOTE: smlnj needs MaxOSX10.9.sdk
    #       Use https://github.com/devernay/xcodelegacy to install it.
    # FIXME: polyml

    # Theorem Proving
    coq_8_6

    # Tools
    autojump
    awscli
    coreutils
    gawk
    gnumake
    gnused
    gnutar
    highlight
    rlwrap
    silver-searcher
    tree

    # Web/JSON
    curl
    httpie
    jid
    jq
    wget

  ]) ++ (with pkgs.haskellPackages; [

    cabal2nix
    intero
    pandoc
    pointfree
    pointful

  ]);

  services.nix-daemon.enable = true;
  services.nix-daemon.tempDir = "/nix/tmp";

  # Recreate /run/current-system symlink after boot.
  services.activate-system.enable = true;

  # programs.nix-script.enable = true;

  programs.emacs.enable = true;

  programs.emacs.emacsConfig = ''
    (setq load-path
          (append '("~/.nix-profile/share/emacs/site-lisp"
	            "/run/current-system/sw/share/emacs/site-lisp")
          load-path))
  '';

  programs.zsh.enable = true;
  programs.zsh.enableBashCompletion = true;

  programs.zsh.variables.cfg = "$HOME/.nixpkgs/darwin-config.nix";
  programs.zsh.variables.darwin = "$HOME/.nix-defexpr/darwin";
  programs.zsh.variables.pkgs = "$HOME/.nix-defexpr/nixpkgs";

  programs.zsh.promptInit = ''
    autoload -U promptinit && promptinit
    PROMPT='%B%(?..%? )%b⇒ '
    RPROMPT='%F{green}%~%f'

    # Configure autojump
    source /run/current-system/sw/share/autojump/autojump.zsh
    autoload -U compinit && compinit -u
  '';

  programs.zsh.loginShellInit = ''
    reexec() {
      echo "reexecuting shell: $SHELL" >&2
      __ETC_ZSHRC_SOURCED= \
      __ETC_ZSHENV_SOURCED= \
      __ETC_ZPROFILE_SOURCED= \
        exec $SHELL -l
    }
  '';

  programs.zsh.interactiveShellInit = ''
    bindkey -e
    setopt AUTOCD
  '';

  programs.fish.enable = false;

  programs.fish.variables.cfg = "$HOME/.nixpkgs/darwin-config.nix";
  programs.fish.variables.darwin = "$HOME/.nix-defexpr/darwin";
  programs.fish.variables.pkgs = "$HOME/.nix-defexpr/nixpkgs";
  # programs.fish.variables.pkgs = "$HOME/src/NixOS/nixpkgs";

  programs.fish.interactiveShellInit = ''
    # TODO: programs.fish.interactiveShellInit

    function hicat -d 'A hackish hicat clone via highlight'
      highlight -O xterm256 $argv | less -cR
    end
  '';

  environment.shellAliases.gpg = "gpg2";
  environment.shellAliases.k = "clear";
  environment.shellAliases.l = "ls -Glah";
  environment.shellAliases.ll = "ls -Glh";
  environment.shellAliases.ls = "ls -G";

  nix.nixPath = [ # Use local nixpkgs checkout instead of channels.
    "darwin=$HOME/.nix-defexpr/darwin"
    "darwin-config=$HOME/.nixpkgs/darwin-configuration.nix"
    "nixpkgs=$HOME/.nix-defexpr/nixpkgs"
    # "/nix/var/nix/profiles/per-user/root/channels"
  ];

  nix.maxJobs = 8;
  nix.buildCores = 4;
  # FIXME: nix.useSandbox = "relaxed"; # NOTE: for testing

  # nix.requireSignedBinaryCaches = false; # HACK
  nixpkgs.config.allowUnfree = true;
  # nixpkgs.config.allowBroken = true; # HACK

  nixpkgs.config.packageOverrides = pkgs: {
  };
}
