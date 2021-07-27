{ ... }:

{

  programs.fish.shellAbbrs = {
    aal = "aws-azure-login --no-prompt";
    kc = "kubectl";
    kcd = "kubectl drain --delete-emptydir-data --ignore-daemonsets";
    kcnp = "kubectl get pods --field-selector=spec.nodeName=";
    kcx = "kubectl --context";
    kg = "kubectl get";
    kgy = "kubectl get -o yaml";
    kn = "kubens";
    kns = "kubens";
    krr = "kubectl rollout restart";
    krs = "kubectl rollout status";
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
