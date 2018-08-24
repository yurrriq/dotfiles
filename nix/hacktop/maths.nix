{ config, pkgs, ... }:

{
  environment = {
    pathsToLink = [
      "/share/gap"
    ];

    systemPackages = with pkgs; [
      gap
      # gap4r8p8
      # gnuplot
      graphviz
      # FIXME: R
    ];
  };

  nixpkgs.config.packageOverrides = super: rec {
    gap4r8p8 = super.callPackage ./pkgs/applications/science/math/gap/4r8p8.nix {};
    pygmentsGAP = with super.python27Packages; buildPythonPackage rec {
      pname = "GAPLexer";
      version = "1.1";
      name = "${pname}-${version}";

      src = super.fetchFromGitHub {
        owner = "yurrriq";
        repo = "gap-pygments-lexer";
        rev = "034ef506e4bb6a09cafa3106be0c8d8aab5ce091";
        sha256 = "11bcwdl1019psvqb13fbgacr7z9y51dw78mnqq975fbiglqy88r1";
      };

      propagatedBuildInputs = [
        pygments
      ];
    };
  };
}
