{

  description = "My (semi-)literate, Nix-based dotfiles";

  inputs = {
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs-stable";
    };
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixgl = {
      url = "github:guibou/nixGL";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nixpkgs.url = "github:nixos/nixpkgs/release-23.05";
    nixpkgs-stable.url = "github:nixos/nixpkgs/release-23.05";
    nur.url = "github:nix-community/nur";
    treefmt-nix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:numtide/treefmt-nix";
    };
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
          self.nixosModules.home-manager
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
    in
    {
      overlays = {
        default =
          lib.composeManyExtensions
            (lib.attrValues
              (lib.filterAttrs (name: _: name != "default") self.overlays));
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
      nixosModules = {
        bootyjams = import ./modules/bootyjams.nix;
        home-manager = {
          home-manager = {
            sharedModules =
              let
                excluded = [
                  "bugwarrior"
                  "nix"
                  "screen-locker"
                  "taskwarrior"
                ];
                notExcluded = lib.filterAttrs (name: _: !(builtins.elem name excluded));
              in
              builtins.attrValues (notExcluded self.homeManagerModules);
            useGlobalPkgs = true;
            useUserPackages = true;
            verbose = true;
          };
        };
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
            "lens"
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
            self.overlays.default
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
      homeManagerModules =
        let
          inherit (builtins) attrNames baseNameOf filter listToAttrs pathExists readDir toPath;
          inherit (lib.attrsets) nameValuePair;
          inherit (lib.strings) hasSuffix removeSuffix;
          resolveModule = relativePath:
            nameValuePair
              (removeSuffix ".nix" (baseNameOf relativePath))
              (import (toPath (./config + ("/" + relativePath))));
          isModule = path:
            hasSuffix ".nix" path ||
            pathExists (./config + ("/" + path + "/default.nix"));
        in
        listToAttrs
          (map resolveModule
            (filter isModule
              (attrNames (readDir ./config))));
      homeConfigurations.eric = inputs.home-manager.lib.homeManagerConfiguration {
        modules = self.nixosModules.home-manager.home-manager.sharedModules ++ [
          self.nixosModules.nixRegistry
          ./machines/sruxps/home.nix
          {
            home = {
              username = "eric";
              homeDirectory = "/home/eric";
              stateVersion = "23.05";
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
    } // inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import inputs.nixpkgs {
          overlays = [
            inputs.emacs-overlay.overlay
            self.overlays.home-manager
            self.overlays.noweb
          ];
          inherit system;
        };
      in
      {
        devShells = {
          default = pkgs.mkShell {
            inherit (self.packages.${system}.default) FONTCONFIG_FILE;
            buildInputs = with pkgs; [
              biber
              (
                emacsWithPackagesFromUsePackage {
                  alwaysEnsure = true;
                  config = ./config/emacs/init.el;
                }
              )
              editorconfig-checker
              git
              git-lfs
              gnumake
              gnupg
              home-manager
              mkpasswd
              nodePackages.node2nix
              pre-commit
              semver-tool
              sops
              stow
            ] ++ self.packages.${system}.default.nativeBuildInputs;
          };
          xmonad =
            let
              myXMonad =
                pkgs.haskellPackages.callCabal2nix "my-xmonad" ./config/xmonad { };
            in
            pkgs.mkShell {
              buildInputs = with pkgs; [
                cabal-install
                (
                  emacsWithPackagesFromUsePackage {
                    alwaysEnsure = true;
                    config = ./config/xmonad/emacs.el;
                  }
                )
                ghcid
                haskell-language-server
                haskellPackages.hlint
                haskellPackages.ormolu
                haskellPackages.pointfree
                pre-commit
                rnix-lsp
              ] ++ myXMonad.env.nativeBuildInputs;
            };
        };
        formatter = inputs.treefmt-nix.lib.mkWrapper pkgs {
          projectRootFile = "flake.nix";
          programs = {
            deadnix = {
              enable = true;
              no-lambda-arg = true;
              no-lambda-pattern-names = true;
            };
            nixpkgs-fmt.enable = true;
            shellcheck.enable = true;
            shfmt.enable = true;
          };
          settings.formatter = rec {
            deadnix.excludes = nixpkgs-fmt.excludes;
            nixpkgs-fmt.excludes = [
              "machines/*/hardware-configuration.nix"
              "pkgs/development/node-packages/node-env.nix"
              "pkgs/development/node-packages/node-packages.nix"
            ];
            shellcheck = {
              includes = [
                "*.sh"
                ".envrc"
              ];
              options = [
                "--format=tty"
                "--shell=bash"
              ];
            };
            shfmt = {
              includes = [
                "*.sh"
                ".envrc"
              ];
              options = [
                "-i"
                "4"
                "-ci"
                "-s"
                "-w"
              ];
            };
          };
        };
        packages = {
          default = self.packages.${system}.yurrriq-dotfiles;
          yurrriq-dotfiles = pkgs.callPackage ./. { };
        };
      });
}
