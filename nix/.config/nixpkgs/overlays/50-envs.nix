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
      rev = "ed952d43b9caaf7563a615166d4b83d451f91c86";
      sha256 = "0mj3lb64k4ikv47nbglk25zvkc46a33gyz1rfbxvbs1kv4718g5s";
    }) {
      overlays = [ ((import <nur> {}).repos.yurrriq.overlays.nur) ];
    };

    name = "k8s-stable";
    config = {
      eksctl = {
        version = "0.1.38";
        sha256 = "1nhsy4d1a1vh7g2ibcxnzgxnldfyh51hiq4v4vy123487b6ndqd0";
      };
      k8s = {
        version = "1.11.7"; # TODO: 1.11.10
        sha256 = "03dq9p6nwkisd80f0r3sp82vqx2ac4ja6b2s55k1l8k89snfxavf";
      };
      kops = {
        version = "1.11.1"; # TODO: 1.12.2
        sha256 = "0jia8dhawh786grnbpn64hvsdm6wz5p7hqir01q5xxpd1psnzygj";
      };
      helm = {
        version = "2.13.1";
        flavor = "linux-amd64";
        sha256 = "1wyhyxsm7260wjx9lqzg7vhply52m9yb5mcixifx0q4lq3s2pgp4";
      };
      helmfile = {
        # TODO
        # version = "0.79.3";
        # sha256 = "0wgfpidpqyvh41dnw351v91z4szi1s6lqak9li2pmddz1rdkx66v";
        version = "0.73.3";
        sha256 = "1br9d0fq5dncvz6ypzwflr7bscyjryvyqjyf7jvyf36ibl8x8gy8";
        modSha256 = "1ksz1c4j7mhsbq6ifqab04588d48c9glyhr4d3d4jyvi19qhwx1d";
      };
    };
  };

  k8s-legacy = let _nur = import <nur> { pkgs = self; }; in _nur.repos.yurrriq.pkgs.buildK8sEnv {
    inherit (k8s-stable) pkgs;

    name = "k8s-legacy";
    config = k8s-stable.config // {
      helm = {
        flavor = "linux-amd64";
        version = "2.11.0";
        sha256 = "14ap6wm7g9fr1yjx6n382rmnhyn5rf12rxmv7dyypcd2h92cjm8a";
      };
      kops = {
        version = "1.11.0";
        sha256 = "1z67jl66g79q6v5kjy9qxx2xp656ybv5hrc10h3wmzy0b0n30s4n";
      };

    };
  };

  k8s-dev = let _nur = import <nur> { pkgs = self; }; in _nur.repos.yurrriq.pkgs.buildK8sEnv {
    inherit (k8s-stable) pkgs;

    name = "k8s-dev";
    config = k8s-stable.config // {
      helm = {
        flavor = "linux-amd64";
        version = "2.14.2";
        sha256 = "0crnn39wip0ysg8z0084mjjhfyw5cqg7mlgxv5g6g6j14gqnmq2j";
      };
      k8s = {
        version = "1.11.9"; # TODO: 1.11.10
        sha256 = "0v2d7dg5iw339jxr8dn5jm0kpdqcl82agqgaq8r80rvdl4yi6rwq";
      };
    };
  };

  k8s-next = let _nur = import <nur> { pkgs = self; }; in _nur.repos.yurrriq.pkgs.buildK8sEnv {
    inherit (k8s-stable) pkgs;

    name = "k8s-next";
    config = k8s-dev.config // {
      k8s = {
        # TODO
        # version = "1.12.9";
        # sha256 = "0fxrvwl60wmj30azaw5ws8ilm62fmabz1l9ixpxq3lbrmkzhvx0m";
        version = "1.12.8";
        sha256 = "164g1i3cvdk39n294c9i1vy633xisxlcr4qb57p0q1px1fsbs50c";
      };
      kops = {
        version = "1.12.2";
        sha256 = "0937crwifnld7r5pf5gvab9ibmf8k44dafr9y3hld2p01ijrari1";
      };
    };
  };

}
