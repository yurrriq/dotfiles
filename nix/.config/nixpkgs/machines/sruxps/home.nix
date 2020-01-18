{ lib, pkgs, ... }:

{

  imports = [
    # FIXME: <setup/config/fish.nix>
    # NOTE: aliases are weird
    <setup/config/git.nix>
    <setup/config/i3.nix>
  ];

  _module.args.pkgs = pkgs.lib.mkForce pkgs;

}
