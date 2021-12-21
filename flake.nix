{

  description = "My (semi-)literate, Nix-based dotfiles";

  inputs = {
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-21.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    naal = {
      url = "github:yurrriq/naal";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nixpkgs.url = "github:nixos/nixpkgs/release-21.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs";
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
          self.nixosModules.common
          self.nixosModules.location
          self.nixosModules.nix
          self.nixosModules.nixPath
          self.nixosModules.nixRegistry
          self.nixosModules.nixos
          self.nixosModules.nixpkgs
          self.nixosModules.bootyjams
          self.nixosModules.clis
          self.nixosModules.applications
          self.nixosModules.virtualisation
          (./machines + "/${name}/configuration.nix")
        ];
        system = "x86_64-linux";
      };
      pkgNameElem = names: pkg:
        builtins.elem
          (builtins.parseDrvName
            (if builtins.hasAttr "pname" pkg then pkg.pname
            else if builtins.hasAttr "name" pkg then pkg.name
            else "")).name
          names;
      pkgs = import inputs.nixpkgs {
        overlays = [
          self.overlays.noweb
          self.overlays.unstable
        ];
        system = "x86_64-linux";
      };
      unstable-pkgs = import inputs.nixpkgs-unstable {
        config.allowUnfreePredicate = pkgNameElem [
          "zoom"
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
        noweb = final: prev: {
          noweb = unstable-pkgs.noweb.override {
            icon-lang = unstable-pkgs.icon-lang.override {
              withGraphics = false;
            };
          };
        };
        unstable = final: prev: {
          inherit (unstable-pkgs)
            autojump
            browserpass
            conftest
            delta
            eksctl
            electron_13
            haskellPackages
            kubectx
            kubelogin
            nix-direnv
            pass
            renderizer
            ripgrep
            scc
            signal-desktop
            sops
            starship
            super-productivity
            tomb
            yq
            zoom-us
            zoxide
            ;
          inherit (unstable-pkgs.python3Packages)
            bugwarrior
            ec2instanceconnectcli
            ;
        };
      };
      devShell.x86_64-linux = pkgs.mkShell {
        inherit (self.defaultPackage.x86_64-linux) FONTCONFIG_FILE;
        buildInputs = with pkgs; [
          biber
          cargo
          git
          gitAndTools.pre-commit
          gnumake
          gnupg
          mkpasswd
          nixpkgs-fmt
          nodePackages.node2nix
          python3Packages.pygments
          # FIXME: python2Packages.pywatchman
          shellcheck
          shfmt
          sops
          stow
        ] ++ self.defaultPackage.x86_64-linux.nativeBuildInputs;
      };
      devShells.x86_64-linux = {
        xmonad =
          let
            _pkgs = import inputs.nixpkgs-unstable {
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
        applications = import ./modules/applications.nix;

        bootyjams = import ./modules/bootyjams.nix;

        clis = import ./modules/clis.nix;

        common = import ./modules/common.nix;

        location = import ./modules/location.nix;

        nix = import ./modules/nix.nix;
        nixPath = {
          nix.nixPath = lib.mapAttrsToList (n: v: "${n}=${v}")
            (lib.filterAttrs (n: _: n != "self") inputs) ++ [
            "nixos-config=/etc/nixos/configuration.nix"
          ];
        };
        nixRegistry = {
          nix.registry = {
            home-manager.flake = inputs.home-manager;
            nixpkgs.flake = inputs.nixpkgs;
            nixpkgs-unstable.flake = inputs.nixpkgs-unstable;
            nur.flake = inputs.nur;
          };
        };
        nixos = import ./modules/nixos.nix;
        nixpkgs = {
          nixpkgs.config.allowUnfreePredicate = pkgNameElem [
            "lastpass-password-manager"
            "reaper"
            "slack"
            "spotify"
            "spotify-unwrapped"
            "steam"
            "steam-original"
            "steam-runtime"
          ];
          nixpkgs.overlays = [
            self.overlay
            inputs.emacs-overlay.overlay
            inputs.naal.overlays.naal
            inputs.nur.overlay
          ];
        };
        virtualisation = import ./modules/virtualisation.nix;
      };
      nixosConfigurations = {
        "nixps" = mkSystem "nixps" "dell-xps-15-9560-intel";
        "MSP-EBAILEY01" = mkSystem "sruxps" "dell-xps-13-7390";
      };
    };

}
