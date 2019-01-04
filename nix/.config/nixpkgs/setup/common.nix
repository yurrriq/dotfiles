{ pkgs, ... }:

{

  imports = [
    ./emacs.nix
    ./fish.nix
    ./tmux.nix
  ];

  environment = {

    pathsToLink = [
      "/share/doc/task"
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
      # FIXME: "https://yurrriq.cachix.org"
      # FIXME: "https://yurrriq-nur-packages.cachix.org"
    ];

    binaryCachePublicKeys = [
      # "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      # FIXME: "yurrriq.cachix.org-1:evpJ5wKluf7QOCcv69VkIxCOtHgubrqXlZpp3JAXLBE="
      # FIXME: "yurrriq-nur-packages.cachix.org-1:7kbjuGBUZcWf876g2cdelmIQXrXzOhpMVBqYOyyAv70="
    ];
  };

  programs = {

    bash.enableCompletion = true;

    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };

  };

}
