{

  description = "My (semi-)literate, Nix-based dotfiles";

  inputs = {
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixpkgs-stable.follows = "nixpkgs-stable";
      };
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils.url = "github:numtide/flake-utils";
    git-hooks-nix = {
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:cachix/git-hooks.nix";
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixgl = {
      url = "github:guibou/nixGL";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nixpkgs.url = "github:nixos/nixpkgs/release-24.11";
    nixpkgs-stable.url = "github:nixos/nixpkgs/release-24.11";
    nur = {
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.treefmt-nix.follows = "treefmt-nix";
      url = "github:nix-community/nur";
    };
    treefmt-nix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:numtide/treefmt-nix";
    };
  };

  nixConfig = {
    commit-lockfile-summary = "build(deps): nix flake update";
  };

  outputs = { flake-parts, self, ... }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.git-hooks-nix.flakeModule
        inputs.treefmt-nix.flakeModule
        ./config/xmonad/flake-module.nix
      ];
      flake = let inherit (inputs.nixpkgs) lib; in {
        lib = {
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
        };
        overlays = {
          default =
            lib.composeManyExtensions
              (lib.attrValues
                (lib.filterAttrs (name: _: name != "default") self.overlays));
          iosevka-custom = final: prev: {
            # https://typeof.net/Iosevka/customizer
            iosevka-custom = prev.iosevka.override {
              privateBuildPlan = ''
                [buildPlans.Iosevkacustom]
                family = "Iosevka Custom"
                spacing = "normal"
                serifs = "sans"
                exportGlyphNames = true
                [buildPlans.Iosevkacustom.weights.Regular]
                shape = 400
                menu = 400
                css = 400
                [buildPlans.Iosevkacustom.weights.Bold]
                shape = 700
                menu = 700
                css = 700
                [buildPlans.Iosevkacustom.slopes.Upright]
                angle = 0
                shape = "upright"
                menu = "upright"
                css = "normal"
                [buildPlans.Iosevkacustom.slopes.Italic]
                angle = 9.4
                shape = "italic"
                menu = "italic"
                css = "italic"
                [buildPlans.Iosevkacustom.ligations]
                inherits = "dlig"
                [buildPlans.Iosevkacustom.variants.design]
                lower-lambda = "curly-tailed-turn"
              '';
              set = "custom";
            };
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
            nixpkgs.config.allowUnfreePredicate = self.lib.pkgNameElem [
              "Oracle_VirtualBox_Extension_Pack"
              "lastpass-password-manager"
              "lens"
              "nvidia"
              "reaper"
              "slack"
              "spotify"
              "spotify-unwrapped"
              "steam"
              # "steam-original"
              # "steam-run"
              "steam-unwrapped"
              "vault"
              "zoom"
            ];
            nixpkgs.overlays = [
              self.overlays.default
              inputs.emacs-overlay.overlay
              inputs.nixgl.overlay
              inputs.nur.overlays.default
            ];
          };
          virtualisation = import ./modules/virtualisation.nix;
        };
        nixosConfigurations = {
          "nixps" = self.lib.mkSystem "nixps" "dell-xps-15-9560-intel";
          "MSP-EBAILEY01" = self.lib.mkSystem "sruxps" "dell-xps-13-7390";
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
                stateVersion = "24.11";
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
      systems = [
        "x86_64-linux"
      ];
      perSystem = { config, pkgs, self', system, ... }: {
        _module.args.pkgs = import inputs.nixpkgs {
          overlays = [
            inputs.emacs-overlay.overlay
            self.overlays.home-manager
            self.overlays.iosevka-custom
            self.overlays.noweb
          ];
          inherit system;
        };
        devShells = {
          default = pkgs.mkShell {
            inherit (self.packages.${system}.default) FONTCONFIG_FILE;
            inputsFrom = [
              config.pre-commit.devShell
              self'.packages.default
            ];
            nativeBuildInputs = with pkgs; [
              biber
              (
                emacsWithPackagesFromUsePackage {
                  alwaysEnsure = true;
                  config = ./config/emacs/init.el;
                }
              )
              git
              git-lfs
              gnumake
              gnupg
              home-manager
              mkpasswd
              nodePackages.node2nix
              semver-tool
              sops
              stow
            ];
          };
        };
        packages = {
          default = self.packages.${system}.yurrriq-dotfiles;
          inherit (pkgs) iosevka-custom;
          yurrriq-dotfiles = pkgs.callPackage ./. { };
        };
        pre-commit.settings.hooks = {
          biber-format = {
            description = "Use Biber to format .bib files";
            enable = true;
            entry =
              let
                biber-format = pkgs.writeShellApplication {
                  name = "biber-format";
                  runtimeInputs = with pkgs; [
                    biber
                    moreutils
                  ];
                  text = ''
                    biber \
                      --nolog \
                      --output-align \
                      --output-fieldcase=lower \
                      --output-file /dev/stdout \
                      --output-resolve \
                      --output-safechars \
                      --quiet --quiet \
                      --tool \
                      "$1" |
                      head -n-1 |
                      sponge "$1"
                  '';
                };
              in
              "${biber-format}/bin/biber-format";
            files = "\\.bib$";
            language = "system";
            name = "Format .bib files";
          };
          editorconfig-checker.enable = true;
          make-srcs = {
            description = "Ensure Noweb sources are up to date";
            enable = true;
            entry =
              let
                make-srcs = pkgs.writeShellApplication {
                  name = "make-srcs";
                  runtimeInputs = with pkgs; [
                    gnumake
                    noweb
                  ];
                  text = "make srcs";
                };
              in
              "${make-srcs}/bin/make-srcs";
            always_run = true;
            language = "system";
            name = "make srcs";
          };
          treefmt.enable = true;
        };
        treefmt = {
          programs = rec {
            deadnix.enable = true;
            deadnix.excludes = nixpkgs-fmt.excludes;
            deadnix.no-lambda-arg = true;
            nixpkgs-fmt.enable = true;
            nixpkgs-fmt.excludes = [
              "machines/*/hardware-configuration.nix"
              "pkgs/development/node-packages/node-env.nix"
              "pkgs/development/node-packages/node-packages.nix"
            ];
            prettier.enable = false;
            shellcheck.enable = true;
            shellcheck.includes = [
              "*.sh"
              ".envrc"
            ];
            shfmt = {
              enable = true;
              indent_size = 4;
            };
          };
          settings.formatter = {
            shellcheck.options = [
              "--format=tty"
              "--shell=bash"
            ];
          };
        };
      };
    };
}
