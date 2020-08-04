{ ... }:

{

  programs.fish.shellAbbrs = {
    aal = "aws-azure-login --no-prompt";
    kc = "kubectl";
    kcd = "kubectl drain --delete-local-data --ignore-daemonsets";
    kcnp = "kubectl get pods --field-selector=spec.nodeName=";
    kcx = "kubectl --context";
    kgno = "kubectl get nodes";
    kn = "kubens";
    kns = "kubens";
    kt = "stern";
    kx = "kubectx";
    nb = "nix build";
    nbd = "nix build --dry-run";
    nbn = "nix build --no-link";
    nbo = "nix build -o";
    rg = "rg -S";
    rga = "rg --hidden --iglob !.git";
    rgf = "rg -F";
    rgin = "rg -IN";
    rgn = "rg --no-heading";
  };

}
