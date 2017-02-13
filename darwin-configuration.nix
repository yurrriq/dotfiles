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
    ffmpeg
    flac
    fluidsynth
    # FIXME: graphicsmagick
    # FIXME: imagemagick
    lame
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
    gperftools

    # Cryptography
    gnupg

    # Database
    # mysql
    # postgresql
    # sqlite

    # Document Preparation
    asciidoc
    docbook5
    docbook5_xsl
    ghostscript
    groff
    latex2html

    # Engraving
    # FIXME: frescobaldi
    lilypond
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
    graphviz
    # FIXME: R

    # Haskell
    cabal-install
    ghc
    stack

    # JavaScript
    nodejs
    # TODO: planck (create package)

    # JVM
    clojure
    leiningen
    maven
    jdk

    # Libraries
    # libsndfile # NOTE: used by fluidsynth
    # openssl

    # Lisp/Scheme
    # clisp-tip # FIXME: https://github.com/NixOS/nixpkgs/issues/20062
    guile
    # FIXME: racket
    sbcl

    # Messaging
    zeromq

    # Miscellaneous
    # FIXME: calibre
    # FIXME: exercism
    # FIXME: kindlegen

    # .NET
    mono

    # Nix
    nixops
    nix-repl

    # OCaml
    ocaml
    camlp5
    opam

    # Protocol Buffers
    protobuf

    # Python
    python  # NOTE: `python2`
    python3 # NOTE: `python` (not `python3`)
    python35Packages.pip
    python35Packages.pygments

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
    openssh
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
    # ngrok # TODO: 2.x
    # TODO: prometheus
    wget

  ]) ++ (with pkgs.haskellPackages; [
    cabal2nix
    # TODO: idris
    intero
    pandoc
    pointfree
    pointful
    purescript

  ]) ++ (with pkgs.elmPackages; [
    elm
  ]);

  services.nix-daemon.enable = true;
  services.nix-daemon.tempDir = "/nix/tmp";

  # Recreate /run/current-system symlink after boot.
  services.activate-system.enable = true;

  # programs.nix-script.enable = true;

  programs.emacs.enable = true;

  programs.emacs.packages = (self: with {
    elpa = self.elpaPackages;
    melpa = self.melpaPackages;
    org = self.orgPackages;
  };
  # NOTE: Spacemacs seems to get pretty mad at elpa.org here.
  (with org; [ org org-plus-contrib ]) ++
  (with melpa; [
    cask
    elixir-mode
    fill-column-indicator
    flycheck
    # ghc
    helm
    # helm-projectile
    # ido-ubiquitous
    intero
    js2-mode
    json-mode
    markdown-mode
    magit
    # nix-sandbox
    nix-mode
    # projectile
    purescript-mode
    # FIXME: python-mode (bad hash)
    # scss-mode
    web-mode
    # TODO: ws-butler
    yaml-mode
  ]));

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

  nix.distributedBuilds = true;
  nix.buildMachines = [
    # { hostName = "build-slave";
    #   system = "x86_64-linux";
    #   maxJobs = 2;
    # }
    { hostName = "nix-docker";
      system = "x86_64-linux";
      maxJobs = 4;
    }
  ];

  # nix.requireSignedBinaryCaches = false; # HACK
  nixpkgs.config.allowUnfree = true;
  # nixpkgs.config.allowBroken = true; # HACK

  nixpkgs.config.packageOverrides = pkgs: rec {
    camlp5 = pkgs.ocamlPackages.camlp5_6_strict;
    coq = pkgs.coq_8_6;
    erlang = pkgs.erlangR19;
    gcc = pkgs.gcc6;
    jdk = pkgs.openjdk8;
    lilypond = pkgs.lilypondUnstable;
    mono = pkgs.mono46;
    nodejs = pkgs.nodejs-7_x;
    ocaml = pkgs.ocaml_4_03;
    # TODO: postgresql = pks.postgresql96;
    protobuf = pkgs.protobuf3_1;
    # TODO: mysql = mysql57;
  };
}
