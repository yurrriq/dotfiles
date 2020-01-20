{ pkgs, ... }:

{

  home = {
    file.".emacs.d/init.el".source = ./init.el;

    sessionVariables = rec {
      EDITOR = "emacsclient -nw -a \"\"";
      GIT_EDITOR = EDITOR;
      VISUAL = "emacsclient -na \"\" -c";
    };
  };

  programs.emacs = {
    enable = true;

    extraPackages = epkgs: with {
      elpa = epkgs.elpaPackages;
      melpa = epkgs.melpaPackages;
      melpaStable = epkgs.melpaStablePackages;
      org = epkgs.orgPackages;
    }; (with elpa; [
      mmm-mode
    ]) ++ (with melpa; [
      avy
      better-defaults
      # clj-refactor
      # clojure-mode
      # TODO: company-lsp
      # TODO: cquery
      crux
      dhall-mode
      direnv
      dockerfile-mode
      editorconfig
      elixir-mode
      enh-ruby-mode
      # FIXME: erlang
      fill-column-indicator
      fish-mode
      gap-mode
      go-mode
      graphviz-dot-mode
      haskell-mode
      helm-ag
      # TODO: hindent
      hl-todo
      htmlize
      idris-mode
      j-mode
      kubernetes-tramp
      # TODO: lsp-haskell
      # TODO: lsp-mode
      # TODO: lsp-ui
      magit
      markdown-mode
      multiple-cursors
      nix-mode
      paredit
      rainbow-delimiters
      robe
      rust-mode
      rvm
      smex
      tuareg
      use-package
      whitespace-cleanup-mode
      yaml-mode
    ]) ++ (with melpaStable; [
      ess
    ]) ++ (with org; [
    ]);
  };

  services.emacs.enable = ! pkgs.stdenv.isDarwin;

}
