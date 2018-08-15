{ pkgs, ... }:

{

  nixpkgs.config.packageOverrides = super: {
    docker = super.docker-edge;
  };

  users.extraUsers.yurrriq.extraGroups = [ "docker" ];

  virtualisation.docker.enable = true;

}
