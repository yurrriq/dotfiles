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
      g = "git";
      gd = "${g} d";
      gdc = "${g} dc";
      gpg = "gpg2";
      gs = "${g} s";
      k = "clear";
      kc = "kubectl";
      kt = "kubetail";
      l = "ls -Glah";
      ll = "ls -Glh";
      ls = "ls -G";
      # Taskwarrior aliases
      p = "task list";
      pp = tbd;
      t = "task";
      ta = "task add";
      tbd = "task burndown.daily";
      te = "env VISUAL=$EDITOR task edit";
      tm = "task mod";
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

    gc = {
      automatic = true;
      options = "--delete-older-than 45d";
    };

  };

  programs = {

    bash.enableCompletion = true;

    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };

  };

}
