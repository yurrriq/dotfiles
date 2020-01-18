{ lib, pkgs, ... }:

{

  imports = [
    # FIXME: <setup/config/fish.nix>
    # NOTE: aliases are weird
    <setup/config/bat.nix>
    <setup/config/direnv.nix>
    <setup/config/git.nix>
    <setup/config/i3.nix>
    <setup/config/taskwarrior.nix>
  ];

  _module.args.pkgs = pkgs.lib.mkForce pkgs;

}
