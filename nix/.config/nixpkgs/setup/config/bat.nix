{ ... }:

{

  programs.bat = {
    enable = true;
    config = {
      pager = "less -FR";
      style = "changes";
      theme = "Monokai Extended";
    };
  };

}
