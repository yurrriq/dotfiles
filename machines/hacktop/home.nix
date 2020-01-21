{ lib, pkgs, ... }:

let

  gpgKey = "60F0AEB0D089C2911183CAF9D2D7DFEA3D4FB51C";

in

{

  imports = [
    ../../config/bat.nix
    ../../config/direnv.nix
    ../../config/emacs
    # TODO: ../../config/firefox.nix
    ../../config/fish
    ../../config/fzf.nix
    ../../config/git
    (import ../../config/gpg.nix { inherit gpgKey; })
    ../../config/htop.nix
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
