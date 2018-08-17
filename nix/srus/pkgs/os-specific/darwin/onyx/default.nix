{ stdenv, fetchurl, undmg }:

stdenv.mkDerivation rec {
  name = "onyx-${version}";
  version = "3.4.6";

  src = fetchurl {
    url = "https://www.titanium-software.fr/download/1013/OnyX.dmg";
    sha256 = "b79fdec40c82b8c05d50af76f11ddd2187e40d76ddfa0abdfde328255ff21de9";
  };

  buildInputs = [ undmg ];

  setSourceRoot = ''
    sourceRoot=OnyX.app
  '';

  installPhase = ''
    install -m755 -d "$out/Applications/OnyX.app"
    cp -R . "$_"
  '';

  meta = with stdenv.lib; {
    description = "A multifunction utility";
    homepage = https://www.titanium-software.fr/en/onyx.html;
    # TODO: license
    platforms = platforms.darwin;
    maintainers = with maintainers; [ yurrriq ];
  };
}
