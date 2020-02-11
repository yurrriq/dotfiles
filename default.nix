{ pkgs ? import ./nix/nixpkgs.nix
, src ? pkgs.nix-gitignore.gitignoreSource [ ".git/" "docs" ] ./.
}:

pkgs.stdenv.mkDerivation rec {
  pname = "yurrriq-dotfiles";
  version = builtins.readFile ./VERSION;
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

  makeFlags = [
    "PREFIX=${placeholder "out"}"
  ];
}
