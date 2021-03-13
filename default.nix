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
    substituteInPlace ./bin/fix-underscores \
        --replace '/usr/bin/env -S gawk' '${pkgs.gawk}/bin/gawk'
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
