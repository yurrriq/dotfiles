{ pkgs, ... }:

{

  environment.systemPAckages = with pkgs; [
    clang
    gcc
    indent
  ];

}
