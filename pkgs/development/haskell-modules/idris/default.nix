{ mkDerivation, aeson, annotated-wl-pprint, ansi-terminal
, ansi-wl-pprint, array, async, base, base64-bytestring, binary
, blaze-html, blaze-markup, bytestring, cheapskate, code-page
, containers, deepseq, directory, fetchgit, filepath, fingertree
, fsnotify, haskeline, ieee754, mtl, network, optparse-applicative
, parsers, pretty, process, regex-tdfa, safe, split, stdenv, tagged
, tasty, tasty-golden, tasty-rerun, terminal-size, text, time
, transformers, transformers-compat, trifecta, uniplate, unix
, unordered-containers, utf8-string, vector
, vector-binary-instances, zip-archive
}:
mkDerivation {
  pname = "idris";
  version = "1.1.0";
  src = fetchgit {
    url = "https://github.com/idris-lang/Idris-dev.git";
    sha256 = "1jm4lpy640n2m7p316hirn9wz6bpy36vkbn2glj1gn3n5m7hh1s2";
    rev = "70f172c92ada0d57495ccbe477271d699ef7de85";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson annotated-wl-pprint ansi-terminal ansi-wl-pprint array async
    base base64-bytestring binary blaze-html blaze-markup bytestring
    cheapskate code-page containers deepseq directory filepath
    fingertree fsnotify haskeline ieee754 mtl network
    optparse-applicative parsers pretty process regex-tdfa safe split
    terminal-size text time transformers transformers-compat trifecta
    uniplate unix unordered-containers utf8-string vector
    vector-binary-instances zip-archive
  ];
  executableHaskellDepends = [
    base directory filepath haskeline transformers
  ];
  testHaskellDepends = [
    base bytestring containers directory filepath haskeline
    optparse-applicative process tagged tasty tasty-golden tasty-rerun
    time transformers
  ];
  doHaddock = false;
  homepage = "http://www.idris-lang.org/";
  description = "Functional Programming Language with Dependent Types";
  license = stdenv.lib.licenses.bsd3;
}
