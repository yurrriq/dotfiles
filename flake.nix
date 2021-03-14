{
  inputs = {
    home-manager = {
      url = "github:nix-community/home-manager/release-20.09";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # TODO: naal.url = "github:yurrriq/naal";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nixpkgs.url = "github:nixos/nixpkgs/release-20.09";
    nixpkgs-unstable.url = "github:nixos/nixpkgs";
    nur.url = "github:nix-community/NUR";
  };
  outputs = { self, ... }@inputs:
    let
      mkSystem = name: machine: inputs.nixpkgs.lib.nixosSystem {
        modules = [
          (./machines + "/${name}/hardware-configuration.nix")
          inputs.nixos-hardware.nixosModules.${machine}
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
          self.nixosModules.nixos
          self.nixosModules.nixpkgs
          self.nixosModules.packages
          (./machines + "/${name}/configuration.nix")
        ];
        system = "x86_64-linux";
      };
      pkgs = import inputs.nixpkgs {
        overlays = [
          self.overlays.noweb
          self.overlays.unstable
        ];
        system = "x86_64-linux";
      };
      unstable-pkgs = import inputs.nixpkgs-unstable {
        config.allowUnfreePredicate =
          pkg: builtins.elem (builtins.getAttr "pname" pkg) [
            "zoom"
          ];
        system = "x86_64-linux";
      };
    in
    {
      # TODO: overlay = final: prev: {};

      overlays = {
        nodePackages = final: prev: {
          nodePackages =
            let
              _nodePackages = prev.callPackage ./pkgs/development/node-packages {
                inherit (prev) pkgs nodejs;
              };
            in
            prev.nodePackages // _nodePackages // {
              aws-azure-login = _nodePackages.aws-azure-login.override {
                PUPPETEER_SKIP_CHROMIUM_DOWNLOAD = "true";
                buildInputs = [ prev.pkgs.makeWrapper ];
                postInstall = ''
                  wrapProgram "$out/bin/aws-azure-login" \
                      --set PUPPETEER_EXECUTABLE_PATH "${prev.pkgs.chromium}/bin/chromium"
                '';
              };
            };
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
            eksctl
            kubectx
            kubelogin
            pass
            renderizer
            ripgrep
            scc
            signal-desktop
            sops
            starship
            tomb
            yq
            zoom-us
            ;
          inherit (unstable-pkgs.python3Packages)
            bugwarrior
            ec2instanceconnectcli
            ;
        };
      };

      devShell.x86_64-linux = pkgs.callPackage ./shell.nix {
        inherit pkgs;
        yurrriq-dotfiles = self.defaultPackage.x86_64-linux;
      };
      packages.x86_64-linux.yurrriq-dotfiles = self.defaultPackage.x86_64-linux;
      defaultPackage.x86_64-linux = pkgs.callPackage ./default.nix {
        inherit pkgs;
      };
      nixosModules = {
        common = import ./modules/common.nix;
        location = import ./modules/location.nix;
        nix = {
          # TODO: consider mapAttrs
          nix.nixPath = [
            "home-manager=${inputs.home-manager}"
            "nixpkgs=${inputs.nixpkgs}"
            "nixpkgs-unstable=${inputs.nixpkgs-unstable}"
            "nur=${inputs.nur}"
          ];
          nix.registry = {
            home-manager.flake = inputs.home-manager;
            nixpkgs.flake = inputs.nixpkgs;
            nixpkgs-unstable.flake = inputs.nixpkgs-unstable;
            nur.flake = inputs.nur;
          };
        };
        nixos = import ./modules/nixos.nix;
        nixpkgs = {
          nixpkgs.overlays = [
            self.overlays.nodePackages
            self.overlays.noweb
            self.overlays.unstable
            inputs.nur.overlay
          ];
        };
        packages = import ./modules/packages.nix;
      };

      nixosConfigurations = {
        "nixps" = mkSystem "nixps" "dell-xps-15-9560-intel";
        "MSP-EBAILEY01" = mkSystem "sruxps" "dell-xps-13-9380";
      };
    };
}
