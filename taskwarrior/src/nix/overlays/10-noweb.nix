self: super: {
  noweb = super.noweb.override {
    icon-lang = super.icon-lang.override {
      withGraphics = false;
    };
  };
}
