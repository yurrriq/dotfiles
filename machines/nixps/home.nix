{ lib, pkgs, ... }:

{

  imports = [
    ../../config/bat.nix
    ../../config/browserpass.nix
    ../../config/direnv.nix
    ../../config/emacs
    ../../config/firefox.nix
    ../../config/fish
    ../../config/fzf.nix
    ../../config/git
    ../../config/gpg.nix
    ../../config/htop.nix
    ../../config/i3
    ../../config/jq.nix
    ../../config/kitty
    ../../config/man.nix
    ../../config/nixpkgs
    ../../config/rebar3.nix
    # TODO: ../../config/taskwarrior
    # TODO: ../../config/vim.nix
  ];

  home.packages = with pkgs; [
    carla
    reaper
  ];

  _module.args.pkgs = lib.mkForce pkgs;

}
