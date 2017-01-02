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
    # Shell
    fish autojump silver-searcher
    # Web
    curl httpie
    # Git
    git gitAndTools.hub
    # Graphics
    graphicsmagick imagemagick
    # Nix
    nixops # nix-repl
    # Haskell
    ghc cabal-install
  ]) ++ (with pkgs.haskellPackages; [
    pointfree pointful
    cabal2nix
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

  environment.shellAliases.k = "clear";
  environment.shellAliases.l = "ls -G";
  environment.shellAliases.la = "ls -Glah";
  environment.shellAliases.ll = "ls -Glh";

  nix.nixPath = [ # Use local nixpkgs checkout instead of channels.
    "darwin=$HOME/.nix-defexpr/darwin"
    "darwin-config=$HOME/.nixpkgs/darwin-configuration.nix"
    "nixpkgs=$HOME/.nix-defexpr/nixpkgs"
    # "/nix/var/nix/profiles/per-user/root/channels"
  ];

  nixpkgs.config.allowUnfree = true;

  nixpkgs.config.packageOverrides = pkgs: {
    # hub_2_2_9 = pkgs.stdenv.lib.overrideDerivation pkgs.gitAndTools.hub (oldAttrs: {
    #   name = "hub-2.2.9";
    #   version = "2.2.9";
    #   src = pkgs.fetchgit {
    #     url = https://github.com/github/hub.git;
    #     rev = "refs/tags/v2.2.9";
    #     sha256 = "1fv4jb9vsbkscnb79gss2mwnd1yf9jhgzw1mhimhx25xizbx1fck";
    #   };
    # });
  };
}
