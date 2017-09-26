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
    graphicsmagick
    imagemagick
    lame
    timidity

    # BEAM
    beam.interpreters.erlangR19
    # FIXME: erlang

    # C/C++
    cc
    clang
    gcc
    # gperftools

    # Cryptography
    gnupg

    # Database
    # mysql
    # postgresql
    # sqlite

    # Document Preparation
    # asciidoc
    # docbook5
    # docbook5_xsl
    ghostscript
    groff
    latex2html

    # Engraving
    # FIXME: frescobaldi
    my-lilypond
    musescore

    # Git
    git
    git-crypt
    git-lfs
    # TODO: gitAndTools.ghi (add package)
    # NOTE: https://github.com/petervanderdoes/gitflow-avh
    gitAndTools.gitflow
    gitAndTools.hub

    # Go
    # go

    # Graphing/Statistics
    # gnuplot
    # FIXME: graphviz SIGSEGV
    # FIXME: R

    # Haskell
    cabal-install
    ghc
    stack

    # FIXME: Io
    # yajl
    # libevent
    # pcre
    # memcached
    # ode
    # sqlite
    # io

    # JavaScript
    nodejs
    npm2nix

    # TODO: planck (add package)

    # JVM
    # boot
    clojure
    leiningen
    # FIXME: lein-nix-build
    maven
    jdk

    # Libraries
    gmp
    libffi
    # libsndfile # NOTE: used by fluidsynth
    # openssl
    zlib

    # Lisp/Scheme
    # clisp-tip # FIXME: https://github.com/NixOS/nixpkgs/issues/20062
    guile
    # FIXME: racket
    sbcl

    # Messaging
    # zeromq

    # Miscellaneous
    # FIXME: calibre
    cowsay
    exercism
    # FIXME: kindlegen
    skim

    # .NET
    # mono

    # Nix
    # nixops
    # nix-repl
    nix-visualize

    # OCaml
    # ocaml
    # camlp5
    # opam

    # Protocol Buffers
    protobuf

    # Python
    python  # NOTE: `python2`
    python3 # NOTE: `python` (not `python3`)

    # Shell
    # autoenv_fish
    # FIXME: bash
    direnv
    fish

    # SML
    # FIXME: smlnj
    # NOTE: smlnj needs MaxOSX10.9.sdk
    #       Use https://github.com/devernay/xcodelegacy to install it.
    polyml

    # Text Editing
    # emacs # NOTE: use Homebrew for now

    # Theorem Proving
    AgdaStdlib
    coq

    # Tools
    aspell
    aspellDicts.de
    aspellDicts.en
    aspellDicts.es
    aspellDicts.fr
    aspellDicts.it
    aspellDicts.sv
    autojump
    automake
    awscli
    coreutils
    # TODO: csvprintf (add package)
    fpp
    gawk
    gnumake
    gnused
    gnutar
    # FIXME: highlight (g++: command not found)
    htop
    moreutils
    # mosh
    openssh
    # p7zip
    rlwrap
    rsync
    silver-searcher
    # FIXME: sshfs-fuse
    sloccount
    # TODO: thefuck (add package)
    tree
    wakatime
    watch
    watchman
    xorg.lndir

    # Virtualization
    # FIXME: xhyve
    # src/vmm/vmm_mem.c:32:10: fatal error: 'Hypervisor/hv.h' file not found

    # Web/JSON
    curl
    httpie
    html-tidy
    jid
    jq
    # TODO: nginx # NOTE: will need to configure daemon too
    # ngrok # TODO: 2.x
    # TODO: prometheus
    wget
  ]) ++ (with pkgs.beam.packages.erlangR19; [
    elixir
    hex2nix
    lfe
    rebar3-open
  ]) ++ (with pkgs.elmPackages; [
    # elm
  ]) ++ (with pkgs.haskellPackages; [
    Agda
    cabal2nix
    hpack
    idris
    # intero
    pandoc
    pointfree
    pointful
    # FIXME: purescript
    titlecase
  ]) ++ (with pkgs.nodePackages; [
    # aglio
    diff-so-fancy
    # dispatch-proxy
    hicat
    node2nix
    json
    # js-beautify
    # json-minify
    # jsonlint
    resume-cli
    # speed-test
    vmd
  ]) ++ (with pkgs.python27Packages; [
    pywatchman
  ]) ++ (with pkgs.python35Packages; [
    pip
    pygments
    setuptools
  ]);

  services.nix-daemon.enable = true;
  services.nix-daemon.tempDir = "/nix/tmp";

  # Recreate /run/current-system symlink after boot.
  services.activate-system.enable = true;

  # programs.nix-script.enable = true;

  programs.fish.enable = true;

  programs.fish.variables.cfg = "$HOME/.nixpkgs/darwin-config.nix";
  programs.fish.variables.darwin = "$HOME/.nix-defexpr/darwin";
  programs.fish.variables.pkgs = "$HOME/.nix-defexpr/nixpkgs";
  programs.fish.variables.ASPELL_CONF =
    "data-dir /run/current-system/sw/lib/aspell/";

  # programs.fish.shellInit = ''
  #   source ${pkgs.autoenv_fish}/share/autoenv_fish/activate.fish
  # '';

  programs.fish.interactiveShellInit = ''
    # function hicat -d 'Hackish hicat clone via highlight'
    #   highlight -O xterm256 $argv | less -cR
    # end

    eval (direnv hook fish)

    source ${pkgs.autojump}/share/autojump/autojump.fish
  '';

  environment.pathsToLink =
    [ # "/bin"
      # "/lib"
      # "/share/info"
      # "/share/locale"
      "/share/cows"
      "/share/emacs"
      # "/Appications"
    ];

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
    # {
    #   hostName = "build-slave";
    #   system = "x86_64-linux";
    #   maxJobs = 2;
    # }
    {
      hostName = "nix-docker";
      system = "x86_64-linux";
      maxJobs = 2;
      sshKey = "/Users/mohacker/.ssh/docker_rsa";
    }
  ];

  # nix.requireSignedBinaryCaches = false; # HACK
  nixpkgs.config.allowUnfree = true;
  # nixpkgs.config.allowBroken = true; # HACK

  nixpkgs.config.packageOverrides = pkgs: rec {
    # autoenv_fish = pkgs.callPackage ./pkgs/misc/autoenv_fish { };
    # camlp5 = pkgs.ocamlPackages.camlp5_6_strict;
    # camlp5 = pkgs.ocamlPackages.camlp5_6_transitional;
    erlang = pkgs.beam.interpreters.erlangR19.override {
      enableDebugInfo = true;
      installTargets = "install";
      wxSupport = false;
    };
    gcc = pkgs.gcc5; # FIXME
    ghc = haskellPackages.ghc;
    haskellPackages = pkgs.haskell.packages.ghc802.override {
      overrides = self: super: rec {
        idris = pkgs.haskell.lib.dontHaddock super.idris;
      };
    };
    imagemagick = pkgs.imagemagick7;
    jdk = pkgs.openjdk8;
    # FIXME
    # lein-nix-build = pkgs.fetchFromGitHub {
    #   owner = "nix-hackers";
    #   repo = "lein-nix-build";
    #   rev = "98add306b4b86c7f2a106e437901fd276af4631d";
    #   sha256 = "01q2mrfj31fj2ypgvnzrxfp1b2cdr33xv7pdbqdac79zaz3pa27v";
    # };
    mono = pkgs.mono46;
    my-lilypond = pkgs.lilypond-with-fonts.override {
      fonts = with pkgs.openlilylib-fonts; [ improviso lilyjazz ];
    };
    # nixops = pkgs.callPackage ./pkgs/tools/package-management/nixops { };
    nix-visualize = pkgs.callPackage (pkgs.fetchFromGitHub {
      owner = "craigmbooth";
      repo = "nix-visualize";
      rev = "2071fe8deb92cc057371325b840b0100ca31a70a";
      sha256 = "1hyxf5qxz9r170i6v36975kh1r04v1322wr3cdvywczr6mmi01sq";
    }) {
      inherit pkgs;
      pythonPackages = pkgs.python2Packages;
    };
    # TODO: mysql = mysql57;
    nodejs = pkgs.nodejs-7_x;
    nodePackages = pkgs.nodePackages //
      pkgs.callPackage ./pkgs/development/node-packages {
        inherit pkgs;
        inherit nodejs;
      };
    # ocaml = pkgs.ocaml_4_03;
    # TODO: postgresql = pks.postgresql96;
    protobuf = pkgs.protobuf3_1;
    timidity = pkgs.callPackage ./pkgs/tools/misc/timidity {
      inherit (pkgs.darwin.apple_sdk.frameworks) CoreAudio;
    };
  };
}
