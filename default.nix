{ gawk
, iosevka-custom
, makeFontsConf
, nix-gitignore
, noweb
, python3Packages
, stdenv
, texliveSmall
, which
, writableTmpDirAsHomeHook
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
      texliveSmall.withPackages (ps: with ps; [
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
        noweb.tex
        substr
        titlesec
        tkz-base
        todonotes
        tufte-latex
        xetex
        xfor
        xindy
        xstring
      ])
    )
    which
    writableTmpDirAsHomeHook
  ];

  makeFlags = [
    "PREFIX=${placeholder "out"}"
  ];

}
