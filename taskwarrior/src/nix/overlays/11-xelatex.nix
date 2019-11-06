self: super: {
  xelatex-noweb = super.texlive.combine {
    inherit (super) noweb;
    inherit (super.texlive) scheme-small
      # datatool
      datetime
      # dirtytalk
      fmtcount
      framed
      fvextra
      # glossaries
      # glossaries-extra
      hardwrap
      ifplatform
      latexmk
      # mfirstuc
      minted
      # substr
      titlesec
      todonotes
      tufte-latex
      xetex
      # xindy
      # xfor
      xstring
      ;
  };
}
