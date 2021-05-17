{ stdenv, fetchFromGitHub }:

stdenv.mkDerivation rec {
  pname = "fish-kubectl-completions";
  version = "f32c4658";

  src = fetchFromGitHub {
    owner = "evanlucas";
    repo = pname;
    rev = version;
    hash = "sha256-7bZV1bONJggj8c3J2J8DFTrRML0J7HCsddiRFZ6raO8=";
  };
  dontBuild = true;
  dontCheck = true;
  installPhase = ''
    install -m555 -Dt $out/share/fish/vendor_completions.d/ completions/kubectl.fish
  '';
}
