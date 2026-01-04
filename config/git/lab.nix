{ config, lib, pkgs, ... }:

let
  tomlFormat = pkgs.formats.toml { };
in

{
  home.packages = [
    pkgs.lab
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
