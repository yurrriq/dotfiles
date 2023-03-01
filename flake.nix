{

  description = "My (semi-)literate, Nix-based dotfiles";

  inputs = {
    deadnix = {
      url = "github:astro/deadnix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.utils.follows = "flake-utils";
    };
    nixgl = {
      url = "github:guibou/nixGL";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/release-22.11";
    nur.url = "github:nix-community/nur";
  };

  outputs = { self, ... }@inputs:
    let
      inherit (inputs.nixpkgs) lib;
      mkSystem = name: machine: lib.nixosSystem {
        modules = [
          (./machines + "/${name}/hardware-configuration.nix")
          inputs.nixos-hardware.nixosModules.${machine}
          inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
          inputs.home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              verbose = true;
            };
          }
          self.nixosModules.location
          self.nixosModules.nix
          self.nixosModules.nixPath
          self.nixosModules.nixRegistry
          self.nixosModules.nixos
          self.nixosModules.nixpkgs
          self.nixosModules.bootyjams
          self.nixosModules.virtualisation
          (./machines + "/${name}/configuration.nix")
        ];
        system = "x86_64-linux";
      };
      pkgNameElem = names: pkg:
        builtins.elem (lib.getName pkg) names;
      pkgs = import inputs.nixpkgs {
        overlays = [
          inputs.deadnix.overlays.default
          self.overlays.home-manager
          self.overlays.noweb
        ];
        system = "x86_64-linux";
      };
    in
    {
      overlay = lib.composeManyExtensions (lib.attrValues self.overlays);

      overlays = {
        fish-completions = final: prev: {
          fish-kubectl-completions = prev.callPackage ./pkgs/shells/fish/kubectl-completions { };
        };
        home-manager = final: prev: {
          home-manager = inputs.home-manager.packages.${prev.system}.home-manager;
        };
        nixGLWrap = final: prev: {
          lib = prev.lib // {
            nixGLWrap = { pkg, binName ? prev.lib.getName pkg }:
              prev.writeShellScriptBin binName ''
                exec ${final.nixgl.nixGLIntel}/bin/nixGLIntel ${pkg}/bin/${binName} "$@"
              '';
          };
        };
        noweb = final: prev: {
          noweb = prev.noweb.override {
            icon-lang = prev.icon-lang.override {
              withGraphics = false;
            };
          };
        };
      };
      devShells.x86_64-linux = {
        default = pkgs.mkShell {
          inherit (self.defaultPackage.x86_64-linux) FONTCONFIG_FILE;
          buildInputs = with pkgs; [
            biber
            deadnix
            git
            git-lfs
            gitAndTools.pre-commit
            gnumake
            gnupg
            home-manager
            mkpasswd
            nixpkgs-fmt
            nodePackages.node2nix
            semver-tool
            shellcheck
            shfmt
            sops
            stow
          ] ++ self.defaultPackage.x86_64-linux.nativeBuildInputs;
        };
        xmonad =
          let
            _pkgs = import inputs.nixpkgs {
              overlays = [
                inputs.emacs-overlay.overlay
              ];
              system = "x86_64-linux";
            };
            myXMonad =
              _pkgs.haskellPackages.callCabal2nix "my-xmonad" ./config/xmonad { };
          in
          _pkgs.mkShell {
            buildInputs = with _pkgs; [
              cabal-install
              ghcid
              gitAndTools.pre-commit
              haskell-language-server
              haskellPackages.ormolu
              haskellPackages.pointfree
              (
                emacsWithPackagesFromUsePackage {
                  alwaysEnsure = true;
                  config = ./config/xmonad/emacs.el;
                }
              )
            ] ++ myXMonad.env.nativeBuildInputs;
          };
      };
      packages.x86_64-linux = {
        fish-kubectl-completions = pkgs.callPackage ./pkgs/shells/fish/kubectl-completions { };
        yurrriq-dotfiles = pkgs.callPackage ./. { };
      };

      defaultPackage.x86_64-linux = self.packages.x86_64-linux.yurrriq-dotfiles;
      nixosModules = {
        bootyjams = import ./modules/bootyjams.nix;
        location = import ./modules/location.nix;
        nix = import ./modules/nix.nix;
        nixPath = {
          nix.nixPath = lib.mapAttrsToList (n: v: "${n}=${v}")
            (lib.filterAttrs (n: _: n != "self") inputs) ++ [
            "nixos-config=/etc/nixos/configuration.nix"
          ];
        };
        nixRegistry = {
          nix.registry = lib.mapAttrs (_: flake: { inherit flake; }) inputs;
        };
        nixos = import ./modules/nixos.nix;
        nixpkgs = {
          nixpkgs.config.allowUnfreePredicate = pkgNameElem [
            "Oracle_VM_VirtualBox_Extension_Pack"
            "lastpass-password-manager"
            "nvidia"
            "reaper"
            "slack"
            "spotify"
            "spotify-unwrapped"
            "steam"
            "steam-original"
            "steam-run"
            "zoom"
          ];
          nixpkgs.overlays = [
            self.overlay
            inputs.deadnix.overlays.default
            inputs.emacs-overlay.overlay
            inputs.nixgl.overlay
            inputs.nur.overlay
          ];
        };
        virtualisation = import ./modules/virtualisation.nix;
      };
      nixosConfigurations = {
        "nixps" = mkSystem "nixps" "dell-xps-15-9560-intel";
        "MSP-EBAILEY01" = mkSystem "sruxps" "dell-xps-13-7390";
      };
      homeConfigurations.eric = inputs.home-manager.lib.homeManagerConfiguration {
        modules = [
          # FIXME: There's gotta be a better way...
          (
            { pkgs, ... }@args:
            ((import ./machines/sruxps/home.nix) args) //
            self.nixosModules.nixRegistry
          )
          {
            home = {
              username = "eric";
              homeDirectory = "/home/eric";
              stateVersion = "22.11";
            };
          }
        ];
        pkgs = import inputs.nixpkgs {
          inherit (self.nixosModules.nixpkgs.nixpkgs) config;
          overlays = self.nixosModules.nixpkgs.nixpkgs.overlays ++ [
            # https://github.com/nix-community/home-manager/issues/2251#issuecomment-895338427
            (final: prev: {
              kitty = prev.lib.nixGLWrap { pkg = prev.kitty; };
              # FIXME
              # zoom-us = prev.lib.nixGLWrap { pkg = prev.zoom-us; binName = "zoom"; };
            })
          ];
          system = "x86_64-linux";
        };
      };
    };

}
