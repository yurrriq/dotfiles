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

    # Audio/Video
    # FIXME: faac
    ffmpeg
    flac
    fluidsynth
    # FIXME: graphicsmagick # NOTE: graphviz error
    # FIXME: imagemagick
    lame
    # libsndfile # NOTE: used by fluidsynth
    # FIXME: timidity

    # BEAM
    elixir
    erlangR19
    hex2nix
    lfe
    rebar3-open

    # C/C++
    cc
    gcc
    # TODO: gperftools

    # Cryptography
    gnupg

    # Database
    # TODO: mysql
    # TODO: sqlite

    # Document Preparation
    asciidoc
    docbook5
    docbook5_xsl
    ghostscript
    groff
    # TODO: latex2html (create package)

    # Engraving
    # FIXME: frescobaldi
    # TODO: lilypond (add 2.19.x package)
    # FIXME: musescore

    # Git
    git
    git-crypt
    git-lfs
    # TODO: gitAndTools.ghi (create package)
    # TODO: gitAndTools.git-flow (create package)
    gitAndTools.hub

    # Go
    go

    # Graphing/Statistics
    gnuplot
    # FIXME: graphviz # NOTE: ApplicationServices ld error
    # FIXME: R

    # Haskell
    cabal-install
    ghc
    stack

    # JVM
    clojure
    leiningen
    openjdk8

    # Libraries
    # TODO: openssl

    # Lisp/Scheme
    guile
    racket
    sbcl

    # Miscellaneous
    # FIXME: exercism
    # FIXME: kindlegen

    # Nix
    nixops
    nix-repl

    # Shell
    bash
    fish

    # SML
    # FIXME: smlnj
    # NOTE: smlnj needs MaxOSX10.9.sdk
    #       Use https://github.com/devernay/xcodelegacy to install it.
    # FIXME: polyml

    # Theorem Proving
    coq

    # Tools
    autojump
    automake
    awscli
    coreutils
    # TODO: csvprintf (create package)
    # TODO: fswatch # FIXME: update to 1.9.3
    gawk
    gnumake
    gnused
    gnutar
    highlight
    htop
    mosh
    # TODO: p7zip
    rlwrap
    silver-searcher
    # FIXME: sshfs-fuse
    sloccount
    tree
    wakatime # FIXME: update to 6.2.
    watchman

    # Virtualization
    # FIXME: xhyve

    # Web/JSON
    curl
    httpie
    jid
    jq
    # TODO: nginx # NOTE: will need to confugure daemon too
    # TODO: prometheus
    wget

  ]) ++ (with pkgs.haskellPackages; [

    cabal2nix
    # TODO: idris
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

  programs.fish.enable = true;

  programs.fish.variables.cfg = "$HOME/.nixpkgs/darwin-config.nix";
  programs.fish.variables.darwin = "$HOME/.nix-defexpr/darwin";
  programs.fish.variables.pkgs = "$HOME/.nix-defexpr/nixpkgs";
  # programs.fish.variables.pkgs = "$HOME/src/NixOS/nixpkgs";

  programs.fish.interactiveShellInit = ''
    function hicat -d 'Hackish hicat clone via highlight'
      highlight -O xterm256 $argv | less -cR
    end
  '';

  environment.shellAliases.gpg = "gpg2";
  environment.shellAliases.k = "clear";
  environment.shellAliases.l = "ls -Glah";
  environment.shellAliases.ll = "ls -Glh";
  environment.shellAliases.ls = "ls -G";

  # TODO: programs.tmux

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
    coq = pkgs.coq_8_6;
    erlang = pkgs.erlangR19;
    gcc = pkgs.gcc6;
    # TODO: mysql = mysql57;
  };
}
