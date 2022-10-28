{ pkgs, ... }:
{
  home.packages = with pkgs; (
    [
      (aspellWithDicts (dicts: [ dicts.en dicts.es dicts.nb dicts.sv ]))
      ansifilter
      bind
      curl
      deadnix
      httpie
      gnutar
      gzip
      unzip
      clac
      coreutils
      expect
      gawk
      gnumake
      gnused
      moreutils
      ripgrep
      shellcheck
      shfmt
      silver-searcher # TODO: find helm-rg solution
      tree
      watch
      xclip
      yq
      mdcat
      # smos
      nix-output-monitor
      nixpkgs-fmt
      nixpkgs-review
      wirelesstools
    ]
  ) ++ lib.optionals stdenv.isLinux [
    libnotify
    lm_sensors
    mtr
    networkmanager-openconnect
    openconnect
  ];
}
