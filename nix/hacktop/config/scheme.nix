{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    guile
    # FIXME: racket
  ];

}
