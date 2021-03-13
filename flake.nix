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
      pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
      unstable-pkgs = import inputs.nixpkgs-unstable {
        system = "x86_64-linux";
        config.allowUnfree = true;
      };
      yurrriq-dotfiles = pkgs.callPackage ./. {
        inherit pkgs;
        src = self;
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

      devShell.x86_64-linux = import ./shell.nix { inherit pkgs; };
      packages.x86_64-linux.yurrriq-dotfiles = yurrriq-dotfiles;
      defaultPackage.x86_64-linux = yurrriq-dotfiles;
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

      nixosConfigurations."nixps" = inputs.nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          # Use the \href{https://github.com/NixOS/nixos-hardware/tree/master/dell/xps/15-9560}{community-curated optimizations} for \href{https://www.dell.com/support/home/en-us/product-support/product/xps-15-9560-laptop/docs}{XPS 15 9560}, the \href{https://github.com/yurrriq/dotfiles/blob/main/machines/nixps/hardware-configuration.nix}{generated hardware config}, my custom \glspl{module}, and the \href{https://nix-community.github.io/home-manager/index.html\#sec-install-nixos-module}{home-manager} \gls{module}.
          inputs.nixos-hardware.nixosModules.dell-xps-15-9560-intel
          inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
          ./machines/nixps/hardware-configuration.nix
          inputs.home-manager.nixosModules.home-manager
          self.nixosModules.common
          self.nixosModules.location
          self.nixosModules.nix
          self.nixosModules.nixos
          self.nixosModules.nixpkgs
          self.nixosModules.packages
          ./machines/nixps/configuration.nix
        ];
      };

      nixosConfigurations."MSP-EBAILEY01" = inputs.nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          inputs.nixos-hardware.nixosModules.dell-xps-13-9380
          ./machines/sruxps/hardware-configuration.nix
          inputs.home-manager.nixosModules.home-manager
          self.nixosModules.common
          self.nixosModules.location
          self.nixosModules.nix
          self.nixosModules.nixos
          self.nixosModules.nixpkgs
          self.nixosModules.packages
          ./machines/sruxps/configuration.nix
        ];
      };
    };
}
