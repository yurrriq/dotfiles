{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    kops
    kubectx
    kubernetes
    kubernetes-helm
    # minikube
  ];

  nixpkgs.config.packageOverrides = super: {
    kops = super.callPackage ../pkgs/applications/networking/cluster/kops {};
    kubectx = super.callPackage ../pkgs/applications/networking/cluster/kubectx {};
    kubernetes = super.callPackage ../pkgs/applications/networking/cluster/kubernetes {};
    kubernetes-helm = super.callPackage ../pkgs/applications/networking/cluster/helm {};
  };

}
