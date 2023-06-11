{ config, lib, pkgs, ... }:
let
  username = lib.head (lib.attrNames config.home-manager.users);
in
{
  environment.systemPackages =
    lib.optionals config.virtualisation.podman.enable [ pkgs.crun ] ++
    lib.optionals config.virtualisation.libvirtd.enable [ pkgs.virt-manager ];
  programs.dconf.enable = config.virtualisation.libvirtd.enable;
  users.users."${username}".extraGroups = [ "libvirtd" ];
  virtualisation = {
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
    libvirtd.enable = lib.mkDefault false;
  };
}
