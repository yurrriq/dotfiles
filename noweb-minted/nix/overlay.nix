self: super: rec {

  noweb = super.noweb.override {
    icon-lang = super.icon-lang.override {
      withGraphics = false;
    };
  };

  latex-noweb = super.texlive.combine {
    inherit noweb;
    inherit (super.texlive) scheme-small;
  };

}
