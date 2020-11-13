{ lib, pkgs, ... }:

{
  imports = (import <nurpkgs> { }).home-manager.modules ++ [
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
    carla
    clementine
    gnucash
    reaper
    steam
    tomb
    file
    lsof
    fd
    exa
  ];

  _module.args.pkgs = lib.mkForce pkgs;

  programs.starship = {
    enable = true;
    package = nixpkgs-unstable.starship;
    settings = {
      add_newline = false;
      character.symbol = "λ";
      # line_break.disabled = true;
      nix_shell = {
        impure_msg = ""; # "😈";
        # pure_msg = "👼";
      };
      stats.disabled = false;
      time.disabled = false;

    };
  };
}
