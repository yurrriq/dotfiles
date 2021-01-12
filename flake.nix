{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-20.09";
    nixpkgs-unstable.url = "github:nixos/nixpkgs";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nur.url = "github:nix-community/NUR";
    home-manager = {
      url = "github:rycee/home-manager/release-20.09";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # TODO: naal.url = "github:yurrriq/naal";
  };
  outputs = { self, ... }@inputs:
    let
      system = "x86_64-linux";
      unstable-pkgs = import inputs.nixpkgs-unstable {
        inherit system;
        config.allowUnfree = true;
      };
    in
    {
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
                  bugwarrior = unstable-pkgs.python38Packages.bugwarrior;
                  ec2instanceconnectcli = unstable-pkgs.python38Packages.callPackage ./pkgs/development/tools/ec2instanceconnectcli { };
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
          ./machines/sruxps/configuration.nix
          ./machines/sruxps/hardware-configuration.nix
          ./modules/common.nix
          ./modules/location.nix
          ./modules/nixos.nix
          ./modules/packages.nix
          inputs.home-manager.nixosModules.home-manager
        ];
      };
    };
}
