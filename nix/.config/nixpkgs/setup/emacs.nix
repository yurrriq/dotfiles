{ pkgs, ... }:

{

  environment = rec {

    shellAliases = rec {
      e = ''emacsclient -na \"\"'';
      ec = e + " -c";
      et = ''emacsclient -nw -a \"\"'';
    };

    systemPackages = [ pkgs.emacs ];

    variables = with shellAliases; {
      EDITOR = et;
      GIT_EDITOR = et;
      VISUAL = ec;
     };

  };

  services.emacs = {
    enable = ! pkgs.stdenv.isDarwin;
    package = pkgs.emacs;
  };

}
