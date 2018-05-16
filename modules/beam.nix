{ config, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      erlang
      emacsPackages.erlangMode
      lfe
    ] ++ (with beam.packages.erlangR20; [
      hex2nix
      rebar3-open
    ]);
  };

  nixpkgs.config.packageOverrides = super: {
    erlang = super.beam.interpreters.erlangR20.override {
      enableDebugInfo = true;
      installTargets = "install";
      wxSupport = false;
    };
  };
}
