{ pkgs, ... }:

{

  environment.systemPackages = (with pkgs; [
    erlang
  ]) ++ (with pkgs.beam.packages.erlangR20; [
    rebar3-open
  ]);

}
