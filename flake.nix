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
    # TODO
    # sops-nix.url = "github:Mic92/sops-nix";
    # sops-nix.inputs.nixpkgs.follows = "nixpkgs";
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
      devShell.x86_64-linux = import ./shell.nix { inherit pkgs; };
      packages.x86_64-linux.yurrriq-dotfiles = yurrriq-dotfiles;
      defaultPackage.x86_64-linux = yurrriq-dotfiles;
      nixosConfigurations."nixps" = inputs.nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          {
            nixpkgs.overlays = [
              inputs.nur.overlay
              (
                _: _: {
                  inherit (unstable-pkgs)
                    autojump
                    browserpass
                    pass
                    ripgrep
                    scc
                    signal-desktop
                    sops
                    starship
                    tomb
                    yq
                    zoom-us
                    ;
                  noweb = unstable-pkgs.noweb.override {
                    icon-lang = unstable-pkgs.icon-lang.override {
                      withGraphics = false;
                    };
                  };
                }
              )
              (
                self: super: rec {
                  nodePackages = super.nodePackages // super.callPackage ./pkgs/development/node-packages {
                    inherit (super) pkgs nodejs;
                  };
                }
              )
            ];
          }
          # Use the \href{https://github.com/NixOS/nixos-hardware/tree/master/dell/xps/15-9560}{community-curated optimizations} for \href{https://www.dell.com/support/home/en-us/product-support/product/xps-15-9560-laptop/docs}{XPS 15 9560}, the \href{https://github.com/yurrriq/dotfiles/blob/main/machines/nixps/hardware-configuration.nix}{generated hardware config}, my custom \glspl{module}, and the \href{https://nix-community.github.io/home-manager/index.html\#sec-install-nixos-module}{home-manager} \gls{module}.
          inputs.nixos-hardware.nixosModules.dell-xps-15-9560-intel
          inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
          ./machines/nixps/hardware-configuration.nix
          inputs.home-manager.nixosModules.home-manager
          # TODO: inputs.sops-nix.nixosModules.sops
          ./modules/common.nix
          ./modules/location.nix
          ./modules/nixos.nix
          ./modules/packages.nix
          ./machines/nixps/configuration.nix
        ];
      };

      nixosConfigurations."MSP-EBAILEY01" = inputs.nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          {
            nixpkgs.overlays = [
              inputs.nur.overlay
              (
                _: _: {
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
                    ec2instanceconnectcli;
                  noweb = unstable-pkgs.noweb.override {
                    icon-lang = unstable-pkgs.icon-lang.override {
                      withGraphics = false;
                    };
                  };
                }
              )
              (
                self: super: rec {
                  nodePackages =
                    let
                      _nodePackages = super.callPackage ./pkgs/development/node-packages {
                        inherit (super) pkgs nodejs;
                      };
                    in
                    super.nodePackages // _nodePackages // {
                      aws-azure-login = _nodePackages.aws-azure-login.override {
                        PUPPETEER_SKIP_CHROMIUM_DOWNLOAD = "true";
                        buildInputs = [ super.pkgs.makeWrapper ];
                        postInstall = ''
                          wrapProgram "$out/bin/aws-azure-login" --set PUPPETEER_EXECUTABLE_PATH "${super.pkgs.chromium}/bin/chromium"
                        '';
                      };
                    };
                }
              )
            ];
          }
          inputs.nixos-hardware.nixosModules.dell-xps-13-9380
          ./machines/sruxps/hardware-configuration.nix
          inputs.home-manager.nixosModules.home-manager
          ./modules/common.nix
          ./modules/location.nix
          ./modules/nixos.nix
          ./modules/packages.nix
          ./machines/sruxps/configuration.nix
        ];
      };
    };
}
