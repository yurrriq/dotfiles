{ ... }:

{

  programs.bat = {
    enable = true;
    config = {
      map-syntax = "*.yaml.gotmpl:YAML";
      pager = "less -FR";
      style = "changes";
      theme = "Monokai Extended";
    };
  };

}
