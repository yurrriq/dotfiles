{ ... }:

{

  xdg.configFile."rebar3/rebar.config".text = ''
    {plugins, [rebar3_hex]}.
  '';

}
