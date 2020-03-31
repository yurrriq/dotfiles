{ ... }:

{

  programs.fish.shellAbbrs = {
    kc = "kubectl";
    kcd = "kubectl drain --delete-local-data --ignore-daemonsets";
    kcnp = "kubectl get pods --field-selector=spec.nodeName=";
    kns = "kubens";
    kt = "kubetail";
    nb = "nix build";
    nbd = "nix build --dry-run";
    nbn = "nix build --no-link";
    nbo = "nix build -o";
    rga = "rg --hidden --iglob !.git";
    rgf = "rg -F";
    rgi = "rg -i";
    rgn = "rg --no-heading";
    rgs = "rg -S";
  };

}
