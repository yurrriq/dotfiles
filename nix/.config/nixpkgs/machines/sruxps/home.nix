{ lib, pkgs, ... }:

{

  imports = [
    # FIXME: <setup/config/fish.nix>
    # NOTE: aliases are weird
    <setup/config/bat.nix>
    <setup/config/browserpass.nix>
    <setup/config/bugwarrior.nix>
    <setup/config/direnv.nix>
    <setup/config/firefox.nix>
    <setup/config/git.nix>
    <setup/config/gpg.nix>
    <setup/config/htop.nix>
    <setup/config/i3>
    <setup/config/jq.nix>
    <setup/config/kitty.nix>
    <setup/config/man.nix>
    <setup/config/rebar3.nix>
    <setup/config/taskwarrior.nix>
  ];

  _module.args.pkgs = pkgs.lib.mkForce pkgs;

}
