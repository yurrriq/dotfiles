{ lib, pkgs, ... }:

{

  imports = [
    ../../config/bat.nix
    ../../config/direnv.nix
    ../../config/emacs
    # TODO: ../../config/firefox.nix
    ../../config/fish
    ../../config/fzf.nix
    ../../config/git
    ../../config/gpg.nix
    ../../config/htop.nix
    ../../config/jq.nix
    ../../config/kitty
    ../../config/man.nix
    ../../config/nixpkgs
    ../../config/rebar3.nix
    # TODO: ../../config/taskwarrior
    # TODO: ../../config/vim.nix
  ];

  accounts.email.accounts.primary = {
    gpg.key = "60F0AEB0D089C2911183CAF9D2D7DFEA3D4FB51C";
    primary = true;
  };

  home.packages = with pkgs; [
    carla
    reaper
  ];

  _module.args.pkgs = lib.mkForce pkgs;

}
