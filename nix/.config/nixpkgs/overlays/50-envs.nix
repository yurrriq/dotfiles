self: super: {

  beamEnv = self.buildEnv {
    name = "beam";
    paths = with self; [
      erlang
      lfe
    ] ++ (with beamPackages; [
      hex2nix
      rebar3-open
    ]);
  };

  cEnv = self.buildEnv {
    name = "c";
    paths = with self; [
      clang
      gcc
      indent
    ];
  };

  engravingEnv = self.buildEnv {
    name = "engraving";
    paths = with self; ([
      lilypond-with-fonts
      musescore
    ] ++ lib.optionals stdenv.isDarwin [
      skim
    ] ++ lib.optionals stdenv.isLinux [
      qpdfview
    ]);
  };

  yellowdigEnv = self.buildEnv {
    name = "yellowdig";
    paths = with self; [
      clojure
      jdk8
      maven
      rlwrap
      slack
    ];
  };

}
