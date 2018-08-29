{ pkgs, ... }:

let

  gitGUI = with pkgs; if stdenv.isDarwin then sourcetree else git-cola;

in

{

  imports = [
    ./fish.nix
    ./tmux.nix
  ];

  environment = {

    pathsToLink = [
      "/share/gap"
    ];

    shellAliases = rec {
      ag = rgs;
      agn = rgn; # "ag --nogroup";
      agq = rgf; # "ag -Q";
      rgn = "rg --no-heading";
      rga = "rg --hidden --iglob !.git";
      rgi = "rg -i";
      rgf = "rg -F";
      rgs = "rg -S";
      e = ''emacsclient -na ""'';
      ec = e + "-c";
      et = ''emacsclient -nw -a ""'';
      gpg = "gpg2";
      k = "clear";
      kc = "kubectl";
      l = "ls -Glah";
      ll = "ls -Glh";
      ls = "ls -G";
    };

  };

  nix = {

    binaryCaches = [
     # "https://cache.nixos.org"
      "https://yurrriq.cachix.org"
      "https://yurrriq-nur-packages.cachix.org"
    ];

    binaryCachePublicKeys = [
      # "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "yurrriq.cachix.org-1:evpJ5wKluf7QOCcv69VkIxCOtHgubrqXlZpp3JAXLBE="
      "yurrriq-nur-packages.cachix.org-1:7kbjuGBUZcWf876g2cdelmIQXrXzOhpMVBqYOyyAv70="
    ];
  };



  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

}
