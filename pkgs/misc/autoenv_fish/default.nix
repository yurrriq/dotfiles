{ stdenv, fetchFromGitHub, fish }:

stdenv.mkDerivation rec {
  name = "autoenv_fish";
  version = "6aa63b3";

  src = fetchFromGitHub {
    owner = "loopbit";
    repo = "autoenv_fish";
    rev = "${version}";
    sha256 = "0rlwl0ixqyamc0kzjim0sabxrh75gc97ljkfvkphziv1rqhvf63h";
  };

  dontBuild    = true;
  dontPatchELF = true;
  dontStrip    = true;

  installPhase = ''
    install -m755 -Dt $out/share/autoenv_fish activate.fish
  '';

  meta = with stdenv.lib; {
    description = "Directory-based environments for fish shell users";
    longDescription = ''
      Autoenv magic for fish shell!

      Please note that this project is intended to make autoenv available for
      fish shell users, this basically means that this version will always be a
      bit behind the main project. Any bugs, bugfixes and contributions are very
      much appreciated, but keep in mind that any feature requests (unless
      strictly fish-related) should be posted here.
    '';

    license     = licenses.mit;
    maintainers = with maintainers; [ yurrriq ];

    inherit (fish.meta) platforms;
    inherit (src.meta) homepage;
  };
}
