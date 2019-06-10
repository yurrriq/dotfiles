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

  elixirEnv = self.buildEnv {
    name = "elixir";
    paths = with self; [
      elixir_1_8
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

  k8s-stable = let _nur = import <nur> { pkgs = self; }; in _nur.repos.yurrriq.lib.buildK8sEnv {
    pkgs = import (_nur.repos.yurrriq.lib.fetchNixpkgs {
      rev = "19436d38d1ec697a861a537870fb6b06d5aa87c8";
      sha256 = "1ylx7vw2vj3n65mlx7d5w0hi6cwc22caqclj5brvpw9p5idbc0kw";
    }) {
      overlays = [ ((import <nur> {}).repos.yurrriq.overlays.nur) ];
    };

    name = "k8s-stable";
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
        version = "2.13.1";
        sha256 = "0a21xigcblhc9wikl7ilqvs7514ds4x71jz4yv2kvv1zjvdd9i8n";
      };
    };
  };

  k8s-legacy = let _nur = import <nur> { pkgs = self; }; in _nur.repos.yurrriq.pkgs.buildK8sEnv {
    inherit (k8s-stable) pkgs;

    name = "k8s-legacy";
    config = k8s-stable.config // {
      helm = {
        flavor = "darwin-amd64";
        version = "2.11.0";
        sha256 = "1cxmmi7pwp0qrv34ini8gklf9kvqal778q38bkacdlqrnfj0zx05";
      };
    };
  };

  k8s-dev = let _nur = import <nur> { pkgs = self; }; in _nur.repos.yurrriq.pkgs.buildK8sEnv {
    inherit (k8s-stable) pkgs;

    name = "k8s-dev";
    config = k8s-stable.config // {
      helm = {
        flavor = "darwin-amd64";
        version = "2.14.0";
        sha256 = "11lflv9wwvazc3f105q79h84q7b4f9jann786yyplhy0a4kykjf9";
      };
      k8s = {
        version = "1.11.9";
        sha256 = "0v2d7dg5iw339jxr8dn5jm0kpdqcl82agqgaq8r80rvdl4yi6rwq";
      };
    };
  };

  k8s-next = let _nur = import <nur> { pkgs = self; }; in _nur.repos.yurrriq.pkgs.buildK8sEnv {
    inherit (k8s-stable) pkgs;

    name = "k8s-next";
    config = k8s-dev.config // {
      kops = {
        version = "1.12.1";
        sha256 = "0jia8dhawh786grnbpn64hvsdm6wz5p7hqir01q5xxpd1psnzygj";
      };
    };
  };

}
