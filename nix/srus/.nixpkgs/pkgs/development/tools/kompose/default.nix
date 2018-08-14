{ stdenv, buildGoPackage, go, fetchFromGitHub }:

buildGoPackage rec {
  name = "kompose-${version}";
  version = "1.8.0";

  goPackagePath = "github.com/kubernetes/kompose";

  src = fetchFromGitHub {
    owner = "kubernetes";
    repo = "kompose";
    rev = "v${version}";
    sha256 = "0yz0nywhak9xzyikk6rb6rf356418dci8qi291as2c6y1gjzlpzv";
  };

  meta = with stdenv.lib; {
    description = "Go from Docker Compose to Kubernetes ";
    homepage = http://kompose.io;
    license = licenses.apache2;
    maintainers = [ maintainers.yurrriq ];
    inherit (go.meta) platforms;
  };
}
