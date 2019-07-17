self: super: {
  pango = super.pango.overrideAttrs(_: {
    patches = [
      (super.fetchurl {
         url = "https://gitlab.gnome.org/GNOME/pango/commit/546f4c242d6f4fe312de3b7c918a848e5172e18d.patch";
         sha256 = "145q335l91ypvdh0imdf269qz23jm7mbzzwbqxbvbm24al11hqjk";
      })
    ];
  });
}
