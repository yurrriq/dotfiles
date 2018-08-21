{ fetchurl, stdenv, lib, undmg }:

stdenv.mkDerivation rec {
  version = "1.0.87.491.ge2a121fc";
  name = "spotify-mac";

  src = fetchurl {
    url = "https://download.scdn.co/Spotify.dmg";
    sha256 = "0iya7sbynvb3glzkradkn8ip4zwghsba45vgnf5rjd2x0zs2j3yr";
  };

  buildInputs = [ undmg ];

  installPhase = ''
    mkdir -p $out/Applications
    cp -r . $out/Applications/Spotify.app
  '';

  meta = with lib; {
    homepage = https://www.spotify.com/;
    description = "Play music from the Spotify music service";
    license = stdenv.lib.licenses.unfree;
    maintainers = with stdenv.lib.maintainers; [ matthewbauer yurrriq ];
    platforms = platforms.darwin;
  };
}
