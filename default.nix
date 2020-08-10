{ pkgs ? import ./nix/nixpkgs.nix
, src ? pkgs.nix-gitignore.gitignoreSource [ ".git/" "docs" "result" ] ./.
}:

pkgs.stdenv.mkDerivation rec {
  pname = "yurrriq-dotfiles";
  version = builtins.readFile ./VERSION;
  inherit src;

  FONTCONFIG_FILE = pkgs.makeFontsConf {
    fontDirectories = [ pkgs.iosevka ];
  };

  nativeBuildInputs = with pkgs; [
    gawk
    noweb
    python3Packages.pygments
    python3
    which
    xelatex-noweb
  ];

  makeFlags = [
    "PREFIX=${placeholder "out"}"
  ];
}
