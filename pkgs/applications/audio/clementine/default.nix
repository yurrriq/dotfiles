{ stdenv, fetchurl, undmg }:

let
  version = "1.3.1";
in

stdenv.mkDerivation rec {
  name = "clementine-${version}";
  inherit version;

  src = fetchurl {
    url = "https://github.com/clementine-player/Clementine/releases/download/${version}/clementine-${version}.dmg";
    sha256 = "825aa66996237e1d3ea2723b24188ead203f298d0bec89f4c3bc6582d9e63e3a";
  };

  buildInputs = [ undmg ];

  installPhase = ''
    local app=$out/Applications/Clementine.app

    mkdir -p $app
    cp -R . $app
    chmod a+x $app/Contents/MacOS/clementine
  '';

  meta = with stdenv.lib; {
    description = "A modern music player and library organizer";
    homepage =  https://www.clementine-player.org;
    repositories.git = git://github.com/clementine-player/Clementine.git;

    license = licenses.gpl3Plus;

    platforms = platforms.darwin;
    maintainers = with maintainers; [ yurrriq ];
  };
}
