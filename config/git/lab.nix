{ config, lib, pkgs, ... }:

let
  tomlFormat = pkgs.formats.toml { };
in

{
  imports = [
    ../password-store.nix
  ];

  home.packages = [
    pkgs.gitAndTools.lab
  ];
} // lib.mkIf (config.programs.password-store.enable) {
  xdg.configFile."lab/lab.toml".source = tomlFormat.generate "lab-config" {
    core = {
      host = "https://gitlab.sportradar.ag";
      load_token = "pass gitlab.sportradar.ag/token/api";
      user = "e.bailey";
    };
  };
}
