self: super: rec {

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

  fsharpEnv = self.buildEnv {
    name = "fsharp";
    paths = with self; [
      fsharp
      mono58
      vscode-with-extensions
    ];
  };

  haskellEnv = self.buildEnv {
    name = "haskell";
    paths = with self; ([
      cabal-install
      cabal2nix
      ghc
    ] ++ (with haskellPackages; [
      hindent
      hpack
      # FIXME: hpack-convert
      pointfree
      pointful
      styx
      stylish-haskell
    ]));
  };

  k8s-0 = let _nur = import <nur> { pkgs = self; }; in _nur.repos.yurrriq.pkgs.buildK8sEnv {
    pkgs = _nur.repos.yurrriq.lib.pinnedNixpkgs rec {
      rev = "e20ee8a710f3a8ea378bb664c2dbfa32dcf399a7";
      sha256 = "0h063hhywrb4vj9g1lg9dp0r9h5i8b5n923iminnckkxxbr3iap1";
    };

    name = "k8s-0";
    config = {
      k8s = {
        version = "1.11.7";
        sha256 = "03dq9p6nwkisd80f0r3sp82vqx2ac4ja6b2s55k1l8k89snfxavf";
      };
      kops = {
        version = "1.11.1";
        sha256 = "0jia8dhawh786grnbpn64hvsdm6wz5p7hqir01q5xxpd1psnzygj";
      };
      helm = {
        flavor = "darwin-amd64";
        version = "2.12.3";
        sha256 = "0lcnmwqpf5wwq0iw81nlk5fpj4j5p4r6zkrjvbqw5mrjacpa9qf9";
      };
      helmfile = {
        version = "0.54.0";
        sha256 = "0x0kh1dshyygsh22sd5ncbimqx3sl3vn1pdr0spzhfy628rg7lax";
      };
    };
  };

  k8s-1 = let _nur = import <nur> { pkgs = self; }; in _nur.repos.yurrriq.pkgs.buildK8sEnv {
    pkgs = _nur.repos.yurrriq.lib.pinnedNixpkgs {
      rev = "e20ee8a710f3a8ea378bb664c2dbfa32dcf399a7";
      sha256 = "0h063hhywrb4vj9g1lg9dp0r9h5i8b5n923iminnckkxxbr3iap1";
    };

    name = "k8s-1";
    config = k8s-0.config // {
      helm = {
        flavor = "darwin-amd64";
        version = "2.13.1";
        sha256 = "0a21xigcblhc9wikl7ilqvs7514ds4x71jz4yv2kvv1zjvdd9i8n";
      };
    };
  };

  k8s-2 = let _nur = import <nur> { pkgs = self; }; in _nur.repos.yurrriq.pkgs.buildK8sEnv {
    pkgs = _nur.repos.yurrriq.lib.pinnedNixpkgs {
      rev = "e20ee8a710f3a8ea378bb664c2dbfa32dcf399a7";
      sha256 = "0h063hhywrb4vj9g1lg9dp0r9h5i8b5n923iminnckkxxbr3iap1";
    };

    name = "k8s-2";
    config = k8s-0.config // {
      helm = {
        flavor = "darwin-amd64";
        version = "2.11.0";
        sha256 = "1cxmmi7pwp0qrv34ini8gklf9kvqal778q38bkacdlqrnfj0zx05";
      };
    };
  };

  k8s-3 = let _nur = import <nur> { pkgs = self; }; in _nur.repos.yurrriq.pkgs.buildK8sEnv {
    inherit (k8s-1) pkgs;

    name = "k8s-3";
    config = k8s-1.config // {
      k8s = {
        version = "1.11.9";
        sha256 = "0v2d7dg5iw339jxr8dn5jm0kpdqcl82agqgaq8r80rvdl4yi6rwq";
      };
    };
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
