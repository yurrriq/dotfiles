{ pkgs, ... }:

{

  environment = {

    shellAliases = rec {
      e = ''emacsclient -na ""'';
      ec = e + " -c";
      et = ''emacsclient -nw -a ""'';
    };

    systemPackages = [ pkgs.emacs ];

    variables = rec {
      EDITOR = "${pkgs.emacs}/bin/emacsclient -tc";
      VISUAL = EDITOR;
    };

  };


  services.emacs = {
    enable = true;
    package = pkgs.emacs;
  };

}
