{ pkgs, ... }:

{

  users.extraUsers.yurrriq.extraGroups = [ "docker" ];

  virtualisation.docker.enable = true;

}
