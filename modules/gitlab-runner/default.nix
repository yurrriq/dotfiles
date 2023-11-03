{ lib, pkgs, ... }:
let
  inherit (builtins) concatStringsSep;
  nixpkgs-unstable = import (import ../nix/sources.nix).nixpkgs-unstable { };
in
{

  disabledModules = [
    "services/continuous-integration/gitlab-runner.nix"
  ];

  imports = [
    ../gitlab-runner.nix
  ];

  # environment.systemPackages = with pkgs; [ git ];

  services.gitlab-runner = {
    enable = true;
    services = {
      shell = {
        registrationConfigFile = ./gitlab-runner-registration;
        executor = "shell";
        tagList = [ "nix-shell" ];
      };
    };
  };

}
