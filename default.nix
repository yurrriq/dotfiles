{ pkgs ? import <nixpkgs> { }
, src ? pkgs.nix-gitignore.gitignoreSource [ ".git/" "docs" "result" ] ./.
}:

pkgs.stdenv.mkDerivation rec {
  pname = "yurrriq-dotfiles";
  version = builtins.readFile ./VERSION;
  inherit src;

  FONTCONFIG_FILE = pkgs.makeFontsConf {
    fontDirectories = [ pkgs.iosevka ];
  };

  configurePhase = ''
    sed -i '0,/-S gawk -f/s//gawk/' ./bin/fix-underscores
    patchShebangs ./bin/fix-underscores ./noweb-minted/
    sed -i '0,/gawk$/s//gawk -f/' ./bin/fix-underscores
    export PATH=$PWD/bin:$PATH
  '';

  nativeBuildInputs = with pkgs; [
    gawk
    noweb
    python3Packages.pygments
    python3
    which
    (
      texlive.combine {
        inherit noweb;
        inherit (texlive) scheme-small
          braket
          catchfile
          datatool
          datetime
          dirtytalk
          fmtcount
          framed
          frankenstein
          fvextra
          glossaries
          glossaries-extra
          hardwrap
          ifplatform
          latexmk
          mfirstuc
          minted
          substr
          titlesec
          tkz-base
          todonotes
          tufte-latex
          xetex
          xindy
          xfor
          xstring
          ;
      }
    )
  ];

  makeFlags = [
    "PREFIX=${placeholder "out"}"
  ];
}
