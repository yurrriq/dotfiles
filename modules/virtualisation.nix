{ config, lib, pkgs, ... }:
{
  environment.systemPackages = lib.optionals config.virtualisation.podman.enable [ pkgs.crun ];
  virtualisation = {
    docker = {
      enable = lib.mkDefault false;
      liveRestore = lib.mkDefault false;
    };
    podman = {
      enable = lib.mkDefault (!config.virtualisation.docker.enable);
      dockerCompat = lib.mkDefault true;
    };
  };
}
