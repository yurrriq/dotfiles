{
  allowBroken = true;
  allowUnfree = true;
  # packageOverrides = pkgs: rec {
  #   fontforge = pkgs.stdenv.lib.overrideDerivation pkgs.fontforge (oldAttrs: {
  #     version = "20150824";
  #     withPython = false;
  #     src = pkgs.fetchFromGitHub {
  #       owner  = "fontforge";
  #       repo   = "fontforge";
  #       rev    = "20150824";
  #       sha256 = "1jyip3h8kzbk6dxxxzpa28g69n943qcndb5z3b1nmf9543mv1a9y";
  #     };
  #   });
  # };
}
