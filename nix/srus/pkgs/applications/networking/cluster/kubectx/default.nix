{ stdenv, fetchFromGitHub }:

stdenv.mkDerivation rec {
  name = "kubectx-${version}";
  version = "v0.5.0";

  src = fetchFromGitHub {
    owner = "ahmetb";
    repo = "kubectx";
    rev = version;
    sha256 = "18ncg0sjsav71ajnhhhf8qdms7m34mgpm0plfybfmxsmn8b1ryzm";
  };

  dontBuild = true;

  installPhase = ''
    install -Dt $out/bin/ -m755 kubectx kubens
  '';

  meta = with stdenv.lib; {
    description = "Fast way to switch between clusters and namespaces in kubectl";
    license = licenses.asl20;
    inherit (src.meta) homepage;
    maintainers = with maintainers; [ yurrriq ];
    platforms = platforms.unix;
  };
}
