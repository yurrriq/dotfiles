{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    elixir
    erlang
    hex2nix
    rebar3-open
  ];

  nixpkgs.config.packageOverrides = super: {
    beam = super.beam // rec {
      interpreters = super.beam.interpreters // rec {
        erlang = super.beam.interpreters.erlangR20;
        erlangR20 = (super.beam.lib.callErlang ../pkgs/development/interpreters/erlang/R20.nix {}).override {
          enableDebugInfo = true;
         installTargets = "install";
          wxSupport = false;
        };
        # FIXME
        # erlangR21 = (super.beam.lib.callErlang ../pkgs/development/interpreters/erlang/R21.nix {}).override {
        #   enableDebugInfo = true;
        #   installTargets = "install";
        #   wxSupport = false;
        # };
      };
      packages = super.beam.packages // rec {
        erlang = super.beam.packages.erlangR20;
        # FIXME: erlangR21 = super.beam.packagesWith interpreters.erlang;
      };
    };
  };

}
