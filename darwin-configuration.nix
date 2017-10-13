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
    ### Audio/Video ###
    ffmpeg
    flac
    fluidsynth
    # graphicsmagick
    # imagemagick
    lame
    timidity

    ### BEAM ###
    # beam.interpreters.erlangR19
    # FIXME: erlang

    ### C/C++ ###
    cc
    clang
    gcc
    # gperftools

    ### Cryptography ###
    gnupg

    ### Database ###
    # mysql
    # postgresql
    # sqlite

    ### Document Preparation ###
    # asciidoc
    # docbook5
    # docbook5_xsl
    # ghostscript
    # groff
    # latex2html

    ### Engraving ###
    # FIXME: frescobaldi
    # FIXME: my-lilypond # NOTE: (guile_1_8 seem broken...)
    musescore

    ### Git ###
    git
    # git-crypt
    # git-lfs
    # TODO: gitAndTools.ghi (add package)
    # NOTE: https://github.com/petervanderdoes/gitflow-avh
    gitAndTools.gitflow
    gitAndTools.hub

    ### Go ###
    # go

    ### Graphing/Statistics ###
    # gnuplot
    # graphviz
    # FIXME: R

    ### Haskell ###
    # cabal-install
    stack

    ### FIXME: Io ###
    # yajl
    # libevent
    # pcre
    # memcached
    # ode
    # sqlite
    # io

    ### JavaScript ###
    nodejs

    # TODO: planck (add package)

    ### JVM ###
    # boot
    # clojure
    # leiningen
    # FIXME: lein-nix-build
    # maven
    jdk

    ### Libraries ###
    gmp
    libffi
    # openssl
    zlib

    ### Lisp/Scheme ###
    # clisp-tip # FIXME: https://github.com/NixOS/nixpkgs/issues/20062
    guile
    # FIXME: racket
    sbcl

    ### Messaging ###
    # zeromq

    ### Miscellaneous ###
    # FIXME: calibre
    # cowsay
    # exercism
    # FIXME: kindlegen
    skim

    ### .NET ###
    # mono

    ### Nix ###
    nix
    # nixops
    # nix-repl
    # nix-visualize
    nix-prefetch-git

    ### OCaml ###
    # ocaml
    # camlp5
    # opam

    ### Protocol Buffers ###
    protobuf

    ### Python ###
    # python  # NOTE: `python2`
    # python3 # NOTE: `python` (not `python3`)

    ### Shell ###
    # FIXME: bash
    direnv
    fish

    ### SML ###
    # FIXME: smlnj
    # NOTE: smlnj needs MaxOSX10.9.sdk
    #       Use https://github.com/devernay/xcodelegacy to install it.
    # polyml

    ### Text Editing ###
    emacs

    ### Theorem Proving ###
    # FIXME: AgdaStdlib
    # coq

    ### Tools ###
    aspell
    # aspellDicts.de
    aspellDicts.en
    aspellDicts.es
    # aspellDicts.fr
    # aspellDicts.it
    aspellDicts.sv
    autojump
    # automake
    # awscli
    coreutils
    # fpp
    gawk
    gnumake
    gnused
    gnutar
    # highlight
    htop
    m-cli
    moreutils
    # mosh
    openssh
    # p7zip
    rlwrap
    rsync
    silver-searcher
    # FIXME: sshfs-fuse
    # sloccount
    tree
    wakatime
    # watch
    watchman
    xorg.lndir

    ### Virtualization ###
    # FIXME: xhyve
    # src/vmm/vmm_mem.c:32:10: fatal error: 'Hypervisor/hv.h' file not found

    ### Web/JSON ###
    curl
    httpie
    html-tidy
    jid
    jq
    # TODO: nginx # NOTE: will need to configure daemon too
    # ngrok # TODO: 2.x
    # TODO: prometheus
    # wget

    ### Maths ###
    gap
    # gap4r8p8

    ### VoiceHive ###
    # apacheHttpd
    php56
    # mysql55
    netbeans
  ]) ++ (with pkgs.php56Packages; [
    composer
    xdebug
  # ]) ++ (with pkgs.beam.packages.erlangR19; [
    # elixir
    # hex2nix
    # lfe
    # rebar3-open
  # ]) ++ (with pkgs.elmPackages; [
  #   elm
  ]) ++ (with pkgs.haskellPackages; [
    # FIXME: Agda
    # FIXME: cabal2nix # NOTE: conflict with pandoc (and hpack and idris)
    ghc
    hpack
    idris
    # intero
    pandoc
    pointfree
    pointful
    # FIXME: purescript
    # titlecase
  ]) ++ (with pkgs.nodePackages; [
    # aglio
    diff-so-fancy
    # dispatch-proxy
    node2nix
    # js-beautify
    json-minify
    # jsonlint
    # resume-cli
    # speed-test
    vmd
  ]) ++ (with pkgs; with python27Packages; [
    pygments
    pygmentsGAP
    pywatchman
  ]) ++ (with pkgs.python35Packages; [
    # pip
    pygments
    # setuptools
  ]);

  services.nix-daemon.enable = true;
  services.nix-daemon.tempDir = "/nix/tmp";

  # Recreate /run/current-system symlink after boot.
  services.activate-system.enable = true;

  # TODO
  # services.mysql.enable = true;
  # services.mysql.package = pkgs.mysql55;
  # services.mysql.dataDir = "/var/db";

  # programs.nix-script.enable = true;

  programs.fish.enable = true;

  programs.fish.interactiveShellInit = ''
    set fish_path $HOME/.oh-my-fish
    set fish_theme yurrriq
    . $fish_path/oh-my-fish.fish

    source ~/.config/fish/secrets.fish

    eval (${pkgs.direnv}/bin/direnv hook fish)

    source ${pkgs.autojump}/share/autojump/autojump.fish

    set -x MANPATH $MANPATH /usr/share/man /usr/local/share/man /usr/X11/share/man
    set -x MANPATH /run/current-system/sw/share/man $MANPATH

    function pcat
      pygmentize -f terminal -g $argv
    end

    function hicat -d 'Hackish hicat clone via pygments'
      pcat $argv | less -cR
    end

    function playmidi
    	fluidsynth -i ~/lib/arachno-soundfont/Arachno\ SoundFont\ -\ Version\ 1.0.sf2 $argv
    end
  '';

  programs.fish.shellInit = ''
    set -x ASPELL_CONF "data-dir /run/current-system/sw/lib/aspell/"
    set -x EDITOR 'emacsclient -cnw -a ""'
    set -x PATH ~/bin /run/current-system/sw/bin $PATH /usr/local/texlive/2017/bin/x86_64-darwin
    set -x VISUAL $EDITOR
    set fish_greeting
  '';

  environment.pathsToLink =
    [ # "/bin"
      "/lib/aspell"
      # "/share/info"
      # "/share/locale"
      "/share/cows"
      "/share/emacs"
      "/share/gap"
      # "/Appications"
    ];

  environment.shellAliases.agn = "ag --nogroup";
  environment.shellAliases.agq = "ag -Q";
  environment.shellAliases.e   = "ec";
  environment.shellAliases.ec  = ''emacsclient -cna ""'';
  environment.shellAliases.et  = ''emacsclient -cnw -a ""'';
  environment.shellAliases.gpg = "gpg2";
  environment.shellAliases.k   = "clear";
  environment.shellAliases.l   = "ls -Glah";
  environment.shellAliases.ll  = "ls -Glh";
  environment.shellAliases.ls  = "ls -G";

  # TODO: programs.tmux

  nix.gc.automatic = true;

  nix.nixPath = [ # Use local nixpkgs checkout instead of channels.
    "darwin=$HOME/.nix-defexpr/channels/darwin"
    "darwin-config=$HOME/.nixpkgs/darwin-configuration.nix"
    "nixpkgs=$HOME/.nix-defexpr/channels/nixpkgs"
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


  environment.etc."php.d/php.ini".text = ''
    zend_extension = ${pkgs.php56Packages.xdebug}/lib/php/extensions/xdebug.so
    xdebug.remote_enable=on
    xdebug.remote_log="/var/log/xdebug.log"
    xdebug.remote_host=localhost
    xdebug.remote_handler=dbgp
    xdebug.remote_port=9000
  '';

  # nix.requireSignedBinaryCaches = false; # HACK
  nixpkgs.config.allowUnfree = true;
  # nixpkgs.config.allowBroken = true; # HACK

  nixpkgs.config.haskellPackageOverrides = self: super: {
    # FIXME
    # idris = self.callPackage ./pkgs/development/haskell-modules/idris {};
    # dyld: Library not loaded: @rpath/libHSidris-1.1.0-JiCWSzPgxk15LdR3aY8fQf-ghc8.0.2.dylib
    #   Referenced from: /nix/tmp/nix-build-idris-1.1.0.drv-0/Idris-dev-70f172c/libs/prelude/../../dist/build/idris/idris
    #   Reason: image not found
  };

  nixpkgs.config.packageOverrides = super: let self = super.pkgs; in {
    autojump = super.callPackage ./pkgs/tools/misc/autojump {};
    # camlp5 = super.ocamlPackages.camlp5_6_strict;
    # camlp5 = super.ocamlPackages.camlp5_6_transitional;
    erlang = super.beam.interpreters.erlangR19.override {
      enableDebugInfo = true;
      installTargets = "install";
      wxSupport = false;
    };
    gap4r8p8 = super.callPackage ./pkgs/applications/science/math/gap/4r8p8.nix {};
    # NOTE: gcc = super.gcc6;
    # imagemagick = super.imagemagick7;
    jdk = super.openjdk8;
    # FIXME
    # lein-nix-build = super.fetchFromGitHub {
    #   owner = "nix-hackers";
    #   repo = "lein-nix-build";
    #   rev = "98add306b4b86c7f2a106e437901fd276af4631d";
    #   sha256 = "01q2mrfj31fj2ypgvnzrxfp1b2cdr33xv7pdbqdac79zaz3pa27v";
    # };
    m-cli = super.callPackage ./pkgs/tools/misc/m-cli {};
    musescore = super.callPackage ./pkgs/applications/audio/musescore/darwin.nix {};
    # FIXME
    my-lilypond = super.lilypond-with-fonts.override {
      fonts = with super.openlilylib-fonts; [ improviso lilyjazz ];
    };
    # TODO: mysql = mysql57;
    nodejs = super.nodejs-6_x;
    nodePackages = super.nodePackages //
      super.callPackage ./pkgs/development/node-packages {
        inherit (super) pkgs;
        inherit (self) nodejs;
      };
    # ocaml = super.ocaml_4_03;
    php = super.php56.overrideDerivation (old: {
      postInstall = ''
        ${old.postInstall}

        cat <<EOF >$out/etc/php.ini
        zend_extension = ${super.php56Packages.xdebug}/lib/php/extensions/xdebug.so
        xdebug.remote_enable=on
        xdebug.remote_log="/var/log/xdebug.log"
        xdebug.remote_host=localhost
        xdebug.remote_handler=dbgp
        xdebug.remote_port=9000
        EOF
      '';
    });
    # postgresql = super.postgresql96;
    # protobuf = super.protobuf3_1;
    pygmentsGAP = with super.python27Packages; buildPythonPackage rec {
      pname = "GAPLexer";
      version = "1.1";
      name = "${pname}-${version}";

      src = super.fetchFromGitHub {
        owner = "yurrriq";
        repo = "gap-pygments-lexer";
        rev = "034ef506e4bb6a09cafa3106be0c8d8aab5ce091";
        sha256 = "11bcwdl1019psvqb13fbgacr7z9y51dw78mnqq975fbiglqy88r1";
      };

      propagatedBuildInputs = [
        pygments
      ];
    };
    skim = super.callPackage ./pkgs/applications/misc/skim {};
    timidity = super.callPackage ./pkgs/tools/misc/timidity {
      inherit (super.darwin.apple_sdk.frameworks) CoreAudio;
    };
    wakatime = super.callPackage ./pkgs/tools/misc/wakatime {};
  };
}
