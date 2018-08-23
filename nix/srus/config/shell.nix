{ pkgs, ... }:

{

  imports = [
    ./fish.nix
    ./tmux.nix
  ];

  environment = {

    pathsToLink = [
      "/share/cows"
    ];

    shellAliases = {
      agn = "ag --nogroup";
      agq = "ag -Q";
      rgn = "rg --no-heading";
      rgi = "rg -i";
      rgf = "rg -F";
      rgs = "rg -S";
      e = ''emacsclient -na ""'';
      ec = ''emacsclient -cna ""'';
      et = ''emacsclient -nw -a ""'';
      gpg = "gpg2";
      k = "clear";
      kc = "kubectl";
      l = "ls -Glah";
      ll = "ls -Glh";
      ls = "ls -G";
    };

    systemPackages = (with pkgs; [
      autojump
      awscli
      coreutils
      # cowsay
      direnv
      # fortune
      # fpp
      gawk
      gnumake
      gnused
      gnutar
      # htop
      httpie
      # jid
      jq
      moreutils
      m-cli
      # mosh
      # pv
      ripgrep
      silver-searcher
      stow
      tree
      watch
      # watchman
      # wget
      # xorg.lndir
      # xz
      yq
    ]) ++ (with pkgs.haskellPackages; [
      (pkgs.haskell.lib.justStaticExecutables ShellCheck)
    ]);

  };

  nixpkgs.config.packageOverrides = super: {
    autojump = super.callPackage ../pkgs/tools/misc/autojump {};
    m-cli = super.callPackage ../pkgs/os-specific/darwin/m-cli {};
  };

}
