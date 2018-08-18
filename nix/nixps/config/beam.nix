{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    erlang
    lfe
  ] ++ (with beam.packages.erlangR20; [
    hex2nix
    rebar3-open
  ]);

}
