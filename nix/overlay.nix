self: super: rec {

  inherit (import (import ./sources.nix).niv { pkgs = super; }) niv;

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
  };

}
