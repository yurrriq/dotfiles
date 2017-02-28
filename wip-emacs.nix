{
  programs.emacs.enable = false;

  programs.emacs.packages = (self: with {
    elpa = self.elpaPackages;
    melpa = self.melpaPackages;
    org = self.orgPackages;
  };
  # NOTE: Spacemacs seems to get pretty mad at elpa.org here.
  (with org; [ org org-plus-contrib ]) ++
  (with melpa; [
    cask
    elixir-mode
    fill-column-indicator
    flycheck
    # ghc
    helm
    # helm-projectile
    # ido-ubiquitous
    intero
    js2-mode
    json-mode
    markdown-mode
    magit
    # nix-sandbox
    nix-mode
    # projectile
    purescript-mode
    # FIXME: python-mode (bad hash)
    # scss-mode
    web-mode
    # TODO: ws-butler
    yaml-mode
  ]));

  programs.emacs.emacsConfig = ''
    (setq load-path
          (append '("~/.nix-profile/share/emacs/site-lisp"
	            "/run/current-system/sw/share/emacs/site-lisp")
          load-path))
  '';
}
