{ stdenv, fetchFromGitHub }:


stdenv.mkDerivation rec {
  name = "lab-${version}";
  inherit (meta) version;

  src = fetchFromGitHub {
    owner = "yurrriq";
    repo = "lab";
    rev = version;
    sha256 = "156f7wvhwl5bklcjy2gm9j4lbyak0qwhn4pcdshlgbrjl988pg6q";
  };

  dontBuild = true;

  installPhase = ''
    install -dm755 $out/bin
    install -Dt $out/bin/ -m755 lab
  '';

  meta = with stdenv.lib; {
    description = "Like hub, but for GitLab";
    version = "0.0.1";
    inherit (src.meta) homepage;
  };
}
