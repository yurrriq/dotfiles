{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # FIXME: my-lilypond # NOTE: (guile_1_8 seem broken...)
  ];

  nixpkgs.config.packageOverrides = super: rec {
    # FIXME
    my-lilypond = (super.lilypond-with-fonts.override {
      lilypond = super.stdenv.lib.overrideDerivation super.lilypond-unstable (p: rec {
        majorVersion = "2.19";
        minorVersion = "80";
        version="${majorVersion}.${minorVersion}";
        name = "lilypond-${version}";

        src = super.fetchurl {
          url = "http://download.linuxaudio.org/lilypond/sources/v${majorVersion}/lilypond-${version}.tar.gz";
          sha256 = "0lql4q946gna2pl1g409mmmsvn2qvnq2z5cihrkfhk7plcqdny9n";
        };

        configureFlags = p.configureFlags ++ [ "CC=${super.cc}" ];
      });
      fonts = with super.openlilylib-fonts; [ improviso lilyjazz ];
    });
  };
}
