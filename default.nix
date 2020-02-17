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

  configurePhase = ''
    sed -i '0,/-S gawk -f/s//gawk/' ./bin/fix-underscores
    patchShebangs ./bin/fix-underscores ./noweb-minted/
    sed -i '0,/gawk$/s//gawk -f/' ./bin/fix-underscores
    export PATH=$PWD/bin:$PWD/noweb-minted:$PATH
  '';

  makeFlags = [
    "PREFIX=${placeholder "out"}"
  ];
}
