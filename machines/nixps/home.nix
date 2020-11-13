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
    # ../../config/i3
    ../../config/jq.nix
    ../../config/kitty.nix
    ../../config/man.nix
    ../../config/nixpkgs
    ../../config/password-store.nix
    ../../config/rebar3.nix
    ../../config/starship.nix
    ../../config/taskwarrior
    ../../config/xmonad
    # TODO: ../../config/vim.nix
  ];
  accounts.email.accounts.primary = {
    address = "eric@ericb.me";
    gpg.key = "F88372B24A806FF23BCB3A4E2DDDF8606958B3F9";
    # gpg.key = "0CBFCE8BC161F36E2E156A526E5EAB98B1F77A17";
    primary = true;
    realName = "Eric Bailey";
  };
  home.packages = with pkgs; [
    exa
    fd

    # carla
    clementine
    reaper

    steam

    file
    lsof
    tomb
  ];
  _module.args.pkgs = lib.mkForce pkgs;
}
