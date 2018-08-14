{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    elixir
    erlang
    hex2nix
    rebar3-open
  ];

  nixpkgs.config.packageOverrides = super: {

    beam = super.beam // {
      interpreters = super.beam.interpreters // rec {
        erlang = erlangR21;
        erlangR20 = (super.beam.lib.callErlang ../pkgs/development/interpreters/erlang/R20.nix {}).override {
          enableDebugInfo = true;
         installTargets = "install";
          wxSupport = false;
        };
        erlangR21 = (super.beam.lib.callErlang ../pkgs/development/interpreters/erlang/R21.nix {}).override {
          enableDebugInfo = true;
          installTargets = "install";
          wxSupport = false;
        };
      };
    };

  };

}
