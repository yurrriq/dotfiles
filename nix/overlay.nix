self: super: rec {

  inherit (import (import ./sources.nix).niv { pkgs = super; }) niv;

  inherit (import (import ./sources.nix).nixpkgs-unstable { }) nixpkgs-fmt;

  noweb = super.noweb.override {
    icon-lang = super.icon-lang.override {
      withGraphics = false;
    };
  };

  xelatex-noweb = super.texlive.combine {
    inherit noweb;
    inherit (super.texlive) scheme-small
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
  };

}
