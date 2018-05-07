{ config, pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    # boot
    # clojure
    jdk
    # FIXME: lein-nix-build
    leiningen
    # maven
    # TODO: planck (add package)
  ];

  nixpkgs.config.packageOverrides = super: rec {
    jdk = super.openjdk8;
    # FIXME
    # lein-nix-build = super.fetchFromGitHub {
    #   owner = "nix-hackers";
    #   repo = "lein-nix-build";
    #   rev = "98add306b4b86c7f2a106e437901fd276af4631d";
    #   sha256 = "01q2mrfj31fj2ypgvnzrxfp1b2cdr33xv7pdbqdac79zaz3pa27v";
    # };
  };
}
