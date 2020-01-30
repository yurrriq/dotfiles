self: super: rec {

  noweb = super.noweb.override {
    icon-lang = super.icon-lang.override {
      withGraphics = false;
    };
  };

  xelatex-noweb = super.texlive.combine {
    inherit noweb;
    inherit (super.texlive) scheme-small
      braket
      datatool
      datetime
      dirtytalk
      fmtcount
      framed
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
  };

}
