{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    kops
    kubectx
    kubernetes
    kubernetes-helm
    # minikube
  ];

}
