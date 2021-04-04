{ config, lib, pkgs, ... }:
let
  username = lib.head (lib.attrNames config.home-manager.users);
in
{
  environment.systemPackages = with pkgs; [
    nfs-utils
  ];
  fileSystems."/mnt/music" = {
    device = "//rackcity/homes/eric/music";
    fsType = "cifs";
    options = [
      "credentials=/etc/nixos/secrets/bootyjams.club"
      "gid=${toString config.ids.gids.users}"
      "noauto"
      "rw"
      "uid=${toString config.users.users.${username}.uid}"
      "vers=2.0"
      # "x-systemd.device-timeout=5s"
      "x-systemd.idle-timeout=600"
      "x-systemd.mount-timeout=5s"
    ];
  };
}
