{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    cc
    clang
    gcc
    # gperftools
  ];
}
