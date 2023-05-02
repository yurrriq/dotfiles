{ config, lib, pkgs, ... }:
{
  environment.systemPackages = lib.optionals config.virtualisation.podman.enable (with pkgs; [ crun tini ] );
  virtualisation = {
    containers.containersConf.extraConfig = ''
      [containers]
      init_path = "${pkgs.tini}/bin/tini"
    '';
    docker = {
      enable = lib.mkDefault false;
      liveRestore = lib.mkDefault false;
    };
    podman = {
      enable = lib.mkDefault (!config.virtualisation.docker.enable);
      dockerCompat = lib.mkDefault true;
    };
    virtualbox.host.enable = lib.mkDefault false;
    virtualbox.host.enableExtensionPack = lib.mkDefault (config.virtualisation.virtualbox.host.enable);
  };
}
