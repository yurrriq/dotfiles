{ stdenv
, nix-gitignore
, makeFontsConf
, iosevka-custom
, gawk
, noweb
, python3Packages
, texlive
, which
}:

stdenv.mkDerivation rec {
  pname = "yurrriq-dotfiles";
  version = builtins.readFile ./VERSION;
  src = nix-gitignore.gitignoreSource [
    ".git/"
    "docs"
    "result*"
    "machines/*/secrets/"
  ] ./.;

  FONTCONFIG_FILE = makeFontsConf {
    fontDirectories = [ iosevka-custom ];
  };

  configurePhase = ''
    substituteInPlace ./bin/fix-underscores \
        --replace '/usr/bin/env -S gawk' '${gawk}/bin/gawk'
  '';

  nativeBuildInputs = [
    gawk
    noweb
    python3Packages.pygments
    (
      texlive.combine {
        inherit noweb;
        inherit (texlive) scheme-small
          braket
          catchfile
          datatool
          datetime
          dirtytalk
          fancyref
          fmtcount
          framed
          frankenstein
          fvextra
          glossaries
          glossaries-extra
          hardwrap
          ifplatform
          latexmk
          mathpazo
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
    which
  ];

  makeFlags = [
    "PREFIX=${placeholder "out"}"
  ];

}
