{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    clojure
    jdk
    # FIXME: lein-nix-build
    maven
  ];

}
