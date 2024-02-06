(column-number-mode 1)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

(require 'package)

(setq-default frames-only-mode t
              indent-tabs-mode nil
              inhibit-splash-screen t
              package-archives nil
              package-enable-at-startup nil)

(package-initialize)

(load-theme 'wombat)

(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "s-u") 'revert-buffer)

(set-face-attribute 'default nil :family "Iosevka Custom" :height 110)

(eval-when-compile
  (require 'use-package))

(setq-default use-package-always-ensure t)

(use-package company
  :custom
  (company-idle-begin 0.5)
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("M-<" . company-select-first)
        ("M->" . company-select-last)))

(use-package crux
  :config
  (global-set-key (kbd "C-a") 'crux-move-beginning-of-line))

(use-package direnv
  :config
  (direnv-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package emojify)

(use-package fill-column-indicator
  :config
  (setq-default fill-column 80)
  (global-display-fill-column-indicator-mode))

(use-package flycheck)

(use-package haskell-mode
  :hook (haskell-mode . interactive-haskell-mode))

(use-package hl-todo
  :demand
  :config (global-hl-todo-mode t))

(use-package lsp-mode
  :hook ((haskell-mode
          nix-mode)
         . lsp-deferred)
  :commands (lsp lsp-deferred)
  :config
  (advice-add 'lsp :before #'direnv-update-environment)
  ;; (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (setq lsp-modeline-code-actions-enable nil))

(use-package lsp-ui
  :hook (haskell-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-position 'bottom))

(use-package lsp-haskell)

(use-package multiple-cursors
  :demand
  :config (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines))

(use-package nix-mode)

(use-package nyan-mode
  :demand
  :config (nyan-mode 1))

(use-package ormolu
 :hook (haskell-mode . ormolu-format-on-save-mode)
 :bind
 (:map haskell-mode-map
   ("C-c r" . ormolu-format-buffer)))

(use-package paredit
  :hook (emacs-lisp-mode . paredit-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode)

(use-package smex
  :demand
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(use-package whitespace-cleanup-mode
  :config (global-whitespace-cleanup-mode t))

(use-package yaml-mode)
