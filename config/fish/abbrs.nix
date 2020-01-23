{ ... }:

{

  programs.fish.shellAbbrs = rec {
    # Kubernetes
    kc = "kubectl";
    kns = "kubens";
    kt = "kubetail";

    # Nix
    nb = "nix build";
    nbn = "${nb} --no-link";

    # ripgrep
    rga = "rg --hidden --iglob !.git";
    rgf = "rg -F";
    rgi = "rg -i";
    rgn = "rg --no-heading";
    rgs = "rg -S";

    # tree
    trea = "tree -a";
  };

}
