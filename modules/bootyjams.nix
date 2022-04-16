{ config, lib, pkgs, ... }:
let
  username = lib.head (lib.attrNames config.home-manager.users);
in
{
  environment.systemPackages = with pkgs; [
    nfs-utils
  ];
  fileSystems =
    let
      mkMount = path: {
        device = "//192.168.1.147${path}";
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
    in
    {
      "/mnt/music" = mkMount "/homes/eric/music";
      "/mnt/reaper" = mkMount "/REAPER Media";
    };
}
