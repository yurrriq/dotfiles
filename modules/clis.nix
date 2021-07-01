{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; (
    [
      (aspellWithDicts (dicts: [ dicts.en dicts.es dicts.nb dicts.sv ]))
      bind
      curl
      httpie
      cachix
      nixUnstable
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
      xorg.xbacklight
      yq
      playerctl
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