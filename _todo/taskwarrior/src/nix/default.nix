{
  pkgs ? import ./nixpkgs.nix,
  src ? pkgs.nix-gitignore.gitignoreRecursiveSource [".git/"] ../..
}:

pkgs.stdenv.mkDerivation rec {
  pname = "taskwarrior-hooks";
  version = builtins.readFile ../../VERSION;
  inherit src;

  FONTCONFIG_FILE = pkgs.makeFontsConf {
    fontDirectories = [ pkgs.iosevka ];
  };

  buildInputs = with pkgs; [
    noweb
    python36Packages.pygments
    which
    xelatex-noweb
  ];

}
