{ lib, pkgs, ... }:

{

  imports = [
    <setup/config/bat.nix>
    <setup/config/browserpass.nix>
    <setup/config/bugwarrior.nix>
    <setup/config/direnv.nix>
    <setup/config/emacs>
    <setup/config/firefox.nix>
    <setup/config/fish>
    <setup/config/fzf.nix>
    <setup/config/git.nix>
    <setup/config/gpg.nix>
    <setup/config/htop.nix>
    <setup/config/i3>
    <setup/config/jq.nix>
    <setup/config/kitty.nix>
    <setup/config/man.nix>
    <setup/config/nixpkgs>
    <setup/config/rebar3.nix>
    <setup/config/taskwarrior.nix>
  ];

  _module.args.pkgs = lib.mkForce pkgs;

}
