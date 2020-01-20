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

  # fsharpEnv = self.buildEnv {
  #   name = "fsharp";
  #   paths = with self; [
  #     fsharp
  #     mono58
  #     vscode-with-extensions
  #   ];
  # };

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

  k8s-legacy = let _nur = import <nur> { pkgs = self; }; in _nur.repos.yurrriq.pkgs.buildK8sEnv {
    inherit (k8s-stable) pkgs;

    name = "k8s-legacy";
    config = k8s-stable.config // {
      helm = {
        flavor = "linux-amd64";
        # version = "2.11.0";
        # sha256 = "14ap6wm7g9fr1yjx6n382rmnhyn5rf12rxmv7dyypcd2h92cjm8a";
        version = "2.12.1";
        sha256 = "03rsbdbxxn1hfh0pkqjgm68dyvyczxa7msqky9s748afjg9yfkjv";
      };
      kops = {
        version = "1.11.0";
        sha256 = "1z67jl66g79q6v5kjy9qxx2xp656ybv5hrc10h3wmzy0b0n30s4n";
      };
    };
  };

  k8s-stable = let _nur = import <nur> { pkgs = self; }; in _nur.repos.yurrriq.lib.buildK8sEnv {
    pkgs = import (_nur.repos.yurrriq.lib.fetchNixpkgs {
      rev = "99f600917fa6bdea714ae27d40467affd37c0169";
      sha256 = "0p6gaz397x4hplwnywxg5538pqk9p9nls72minrmc306ly07jpf6";
    }) {
      overlays = [ ((import <nur> {}).repos.yurrriq.overlays.nur) ];
    };

    name = "k8s-stable";
    config = {
      eksctl = {
        version = "0.3.0";
        sha256 = "0xbvx3rpd97hc49njvc4mx5zm2x08hrg8kpkapa8qigyajjxz9fg";
      };
      k8s = {
        version = "1.11.7"; # TODO: 1.13.10
        sha256 = "03dq9p6nwkisd80f0r3sp82vqx2ac4ja6b2s55k1l8k89snfxavf";
      };
      kops = {
        version = "1.11.1"; # TODO: 1.13.0
        sha256 = "0jia8dhawh786grnbpn64hvsdm6wz5p7hqir01q5xxpd1psnzygj";
      };
      helm = {
        version = "2.13.1"; # TODO: 2.14.3
        flavor = "linux-amd64";
        sha256 = "1wyhyxsm7260wjx9lqzg7vhply52m9yb5mcixifx0q4lq3s2pgp4";
      };
      # FIXME
      helmfile = {
        version = "0.79.3";
        sha256 = "0wgfpidpqyvh41dnw351v91z4szi1s6lqak9li2pmddz1rdkx66v";
        modSha256 = "0jqss8bgwvggygmr5y2yj98apkqs8z3vmwyfh2f6s67k68h57m57";
      };
    };
  };

  k8s-dev = let _nur = import <nur> { pkgs = self; }; in _nur.repos.yurrriq.pkgs.buildK8sEnv {
    inherit (k8s-stable) pkgs;

    name = "k8s-dev";
    config = k8s-stable.config // {
      helm = {
        flavor = "linux-amd64";
        version = "2.14.3";
        sha256 = "03rad3v9z1kk7j9wl8fh0wvsn46rny096wjq3xbyyr8slwskxg5y";
      };
      k8s = {
        version = "1.12.9";
        sha256 = "0fxrvwl60wmj30azaw5ws8ilm62fmabz1l9ixpxq3lbrmkzhvx0m";
      };
      kops = {
        version = "1.13.0";
        sha256 = "04kbbg3gqzwzzzq1lmnpw2gqky3pfwfk7pc0laxv2yssk9wac5k1";
      };
    };
  };

  k8s-next = let _nur = import <nur> { pkgs = self; }; in _nur.repos.yurrriq.pkgs.buildK8sEnv {
    inherit (k8s-stable) pkgs;

    name = "k8s-next";
    config = k8s-dev.config // {
      k8s = {
        version = "1.13.10";
        sha256 = "0x9blsc0pn77aiaaws45r1531jl29psgljq0l67kpw0bgppg5y7w";
      };
    };
  };

}
