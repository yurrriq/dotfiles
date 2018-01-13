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
  system.defaults.dock.mru-spaces = false;
  system.defaults.dock.orientation = "right";
  system.defaults.dock.showhidden = true;

  system.defaults.finder.AppleShowAllExtensions = true;
  system.defaults.finder.FXEnableExtensionChangeWarning = false;
  system.defaults.finder.QuitMenuItem = true;

  system.defaults.trackpad.Clicking = true;

  # TODO: dig through https://github.com/mathiasbynens/dotfiles/blob/master/.macos

  environment.etc."php.d/php.ini".text = ''
    zend_extension = ${pkgs.php56Packages.xdebug}/lib/php/extensions/xdebug.so
    xdebug.remote_enable=on
    xdebug.remote_log="/var/log/xdebug.log"
    xdebug.remote_host=localhost
    xdebug.remote_handler=dbgp
    xdebug.remote_port=9000
  '';

  environment.loginShell = "${pkgs.fish}/bin/fish";

  environment.pathsToLink = [
    "/lib/aspell"
    "/share/cows"
    "/share/emacs"
    "/share/gap"
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

  environment.systemPackages = (with pkgs; [
    ### Fonts
    # iosevka

    ### Audio/Video ###
    clementine
    ffmpeg
    flac
    fluidsynth
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
    git-crypt
    # git-lfs
    # TODO: gitAndTools.ghi (add package)
    gitAndTools.gitflow
    gitAndTools.hub

    ### Go ###
    # go

    ### Graphing/Statistics ###
    # gnuplot
    graphviz
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
    leiningen
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

    ### Miscellaneous ###
    # FIXME: calibre
    # cowsay
    # exercism
    # FIXME: gnucash
    # FIXME: kindlegen
    skim

    ### Nix ###
    nix
    # nixops
    nix-repl
    nix-prefetch-git

    ### Shell ###
    direnv
    fish

    ### SML ###
    # FIXME: smlnj
    # NOTE: smlnj needs MaxOSX10.9.sdk
    #       Use https://github.com/devernay/xcodelegacy to install it.
    # polyml

    ### Text Editing ###
    emacs

    literate

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
    awscli
    coreutils
    diff-pdf
    fpp
    gawk
    gnumake
    gnused
    gnutar
    # highlight
    htop
    indent
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
    watch
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
    difftodo
    # ghc
    hindent
    hpack
    idris
    # intero
    pandoc
    pointfree
    pointful
    # FIXME: purescript
    stylish-haskell
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
    speed-test
    vmd
  ]) ++ (with pkgs.nodePackages_8_x; [
    heroku-cli
  ]) ++ (with pkgs; with python27Packages; [
    pygments
    pygmentsGAP
    pywatchman
  ]) ++ (with pkgs.python35Packages; [
    # pip
    pygments
    # setuptools
  ]);

  programs.fish.enable = true;
  programs.fish.shellInit = ''
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

    set -x EDITOR 'emacsclient -cnw -a ""'
    set -x PATH ~/bin $PATH /usr/local/texlive/2017/bin/x86_64-darwin
    set -x VISUAL $EDITOR
    test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish
    set fish_greeting
  '';

  programs.nix-script.enable = true;

  programs.tmux.enable = true;
  programs.tmux.iTerm2 = true;
  programs.tmux.tmuxConfig = ''
    set -s escape-time 0
  '';

  services.activate-system.enable = true;

  services.nix-daemon.enable = true;
  services.nix-daemon.tempDir = "/nix/tmp";

  nix.gc.automatic = true;

  nix.nixPath = [
    "darwin=$HOME/.nix-defexpr/channels/darwin"
    "darwin-config=$HOME/.nixpkgs/darwin-configuration.nix"
    "nixpkgs=$HOME/.nix-defexpr/channels/nixpkgs"
    # "/nix/var/nix/profiles/per-user/root/channels"
  ];

  nix.buildCores = 4;
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

  nixpkgs.config.allowUnfree = true;

  nixpkgs.config.haskellPackageOverrides = self: super: {
    # FIXME
    # idris = self.callPackage ./pkgs/development/haskell-modules/idris {};
    # dyld: Library not loaded: @rpath/libHSidris-1.1.0-JiCWSzPgxk15LdR3aY8fQf-ghc8.0.2.dylib
    #   Referenced from: /nix/tmp/nix-build-idris-1.1.0.drv-0/Idris-dev-70f172c/libs/prelude/../../dist/build/idris/idris
    #   Reason: image not found
  };

  nixpkgs.config.packageOverrides = super: let self = super.pkgs; in rec {
    autojump = super.callPackage ./pkgs/tools/misc/autojump {};
    erlang = super.beam.interpreters.erlangR19.override {
      enableDebugInfo = true;
      installTargets = "install";
      wxSupport = false;
    };
    clementine = super.callPackage ./pkgs/applications/audio/clementine {};
    diff-pdf = super.callPackage ./pkgs/tools/text/diff-pdf {
      inherit (super.darwin.apple_sdk.frameworks) Cocoa;
    };
    gap4r8p8 = super.callPackage ./pkgs/applications/science/math/gap/4r8p8.nix {};
    gnucash = super.callPackage ./pkgs/applications/office/gnucash {};
    # idrisPackages = super.callPackage ./pkgs/development/idris-modules {
    #   idris-no-deps =
    #     let
    #       inherit (self.haskell) lib;
    #       haskellPackages = self.haskellPackages.override {
    #         overrides = self: super: {
    #           binary = lib.dontCheck self.binary_0_8_5_1;
    #           cheapskate = self.cheapskate_0_1_1;
    #           idris = self.idris_1_1_1;
    #           parsers = lib.dontCheck super.parsers;
    #           semigroupoids = lib.dontCheck super.semigroupoids;
    #           trifecta = lib.dontCheck super.trifecta;
    #         };
    #       };
    #     in
    #       haskellPackages.idris;
    # };
    # idris = idrisPackages.with-packages [ idrisPackages.base ] ;
    jdk = super.openjdk8;
    # FIXME
    # lein-nix-build = super.fetchFromGitHub {
    #   owner = "nix-hackers";
    #   repo = "lein-nix-build";
    #   rev = "98add306b4b86c7f2a106e437901fd276af4631d";
    #   sha256 = "01q2mrfj31fj2ypgvnzrxfp1b2cdr33xv7pdbqdac79zaz3pa27v";
    # };
    literate = super.callPackage ./pkgs/development/tools/literate-programming/literate {};
    m-cli = super.callPackage ./pkgs/tools/misc/m-cli {};
    musescore = super.callPackage ./pkgs/applications/audio/musescore/darwin.nix {};
    # FIXME
    my-lilypond = (super.lilypond-with-fonts.override {
      lilypond = super.stdenv.lib.overrideDerivation super.lilypond-unstable (p: rec {
        majorVersion = "2.19";
        minorVersion = "80";
        version="${majorVersion}.${minorVersion}";
        name = "lilypond-${version}";

        src = super.fetchurl {
          url = "http://download.linuxaudio.org/lilypond/sources/v${majorVersion}/lilypond-${version}.tar.gz";
          sha256 = "0lql4q946gna2pl1g409mmmsvn2qvnq2z5cihrkfhk7plcqdny9n";
        };

        configureFlags = p.configureFlags ++ [ "CC=${super.cc}" ];
      });
      fonts = with super.openlilylib-fonts; [ improviso lilyjazz ];
    });
    nodejs = super.nodejs-6_x;
    nodePackages = super.nodePackages //
      super.callPackage ./pkgs/development/node-packages {
        inherit (super) pkgs;
        inherit (self) nodejs;
    };
    nodePackages_8_x = super.nodePackages_8_x //
      super.callPackage ./pkgs/development/node-packages-8x {
      inherit (super) pkgs;
      nodejs = super.nodejs-8_x;
    };
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

  nix.distributedBuilds = true;
  nix.maxJobs = 8;
  # nix.requireSignedBinaryCaches = false; # HACK
}
