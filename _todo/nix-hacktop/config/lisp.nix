{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    # clisp-tip # FIXME: https://github.com/NixOS/nixpkgs/issues/20062
    sbcl
  ];

}
