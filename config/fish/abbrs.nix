{ ... }:

{

  programs.fish.shellAbbrs = {
    da = "direnv allow";
    dn = "direnv deny";
    dr = "direnv reload";
    kc = "kubectl";
    kcd = "kubectl drain --delete-emptydir-data --ignore-daemonsets";
    kcn = "kubectl --namespace";
    kcnp = "kubectl get pods --field-selector=spec.nodeName=";
    kcx = "kubectl --context";
    kg = "kubectl get";
    kgp = "kubectl get pods";
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
    nfc = "nix flake check --log-format internal-json --verbose &| nom --json";
    rg = "rg -S";
    rga = "rg --hidden --iglob !.git";
    rgf = "rg -F";
    rgin = "rg -IN";
    rgn = "rg --no-heading";
  };

}
