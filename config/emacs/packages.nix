{ pkgs, ... }:

{

  home.packages = with pkgs; [
    graphviz
    noweb
    sqlite
  ];

  programs.emacs.extraPackages = epkgs: (
    with epkgs.elpaPackages; [
      mmm-mode
      rainbow-mode
    ]
  ) ++ (
    with epkgs.melpaPackages; [
      avy
      better-defaults
      clj-refactor
      clojure-mode
      # TODO: company-lsp
      # TODO: cquery
      crux
      # dhall-mode
      direnv
      dockerfile-mode
      editorconfig
      elixir-mode
      emojify
      # enh-ruby-mode
      erlang
      fill-column-indicator
      fish-mode
      frames-only-mode
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
      lfe-mode
      kubernetes-tramp
      # TODO: lsp-haskell
      # TODO: lsp-mode
      # TODO: lsp-ui
      magit
      markdown-mode
      multiple-cursors
      nix-mode
      nyan-mode
      org-roam
      dash
      f
      s
      emacsql
      emacsql-sqlite3
      paredit
      rainbow-delimiters
      # robe
      rust-mode
      # rvm
      smex
      terraform-mode
      tuareg
      use-package
      whitespace-cleanup-mode
      yaml-mode
    ]
  );

}
