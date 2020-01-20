{ ... }:

{

  xdg.configFile.rebar3 = {
    target = "rebar3/rebar.config";
    text = ''
      {plugins, [rebar3_hex]}.
    '';
  };

}
