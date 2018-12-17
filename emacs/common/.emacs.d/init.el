(server-start)

(setq server-window 'pop-to-buffer-same-window)


(menu-bar-mode 0)

(tool-bar-mode 0)


(let ((font "Iosevka-12"))
  (set-face-attribute 'default t :font font)
  (set-frame-font font nil t))


(require 'package)

(setq-default package-archives nil
	      package-enable-at-startup nil)

(package-initialize)


(eval-when-compile
  (require 'use-package))

(setq-default use-package-always-defer t
	      use-package-always-ensure t)


;; https://stackoverflow.com/a/18330742

(defvar --backup-directory
  (concat user-emacs-directory "backups"))

(unless (file-exists-p --backup-directory)
  (make-directory --backup-directory t))

(add-to-list 'backup-directory-alist `("." . ,--backup-directory))

(setq make-backup-files         t
      backup-by-copying         t
      version-control           t
      delete-old-versions       t
      delete-by-moving-to-trash t
      kept-old-versions         2
      kept-new-versions         6
      auto-save-default         t
      auto-save-timeout         20
      auto-save-interval        200)


(setq custom-file "~/.emacs.d/private/local/custom.el")


(load-theme 'wombat)


(global-set-key (kbd "C-x C-k") 'kill-this-buffer)


(setq c-default-style      "k&r"
      c-basic-offset       4
      emacs-lisp-mode-hook '(fci-mode paredit-mode
			     rainbow-delimiters-mode)
      js-indent-level      2
      text-mode-hook       '(text-mode-hook-identify))


(setq-default fill-column 80)


(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))


(use-package clojure-mode
  :mode ("\\.clj\\'")
  :config
  (dolist (hook emacs-lisp-mode-hook)
    (add-to-list 'clojure-mode-hook hook)))

;; TODO
;; (use-package company-lsp
;;   :init
;;   (use-package lsp-mode)
;;   (use-package lsp-ui)
;;   :after (company lsp-mode)
;;   :config
;;   (push 'company-lsp company-backends))


(use-package crux
  :demand
  :config (global-set-key (kbd "C-a") 'crux-move-beginning-of-line))


(use-package ess
  :demand)


;; TODO
;; (use-package haskell-mode
;;   :diminish (haskell-mode . " ")
;;   :init
;;   ;; (use-package shm
;;   ;;   :hook (haskell-mode . structured-haskell-mode))
;;   (use-package hindent
;;     :hook (haskell-mode . hindent-mode))
;;   (use-package lsp-haskell))


(use-package helm
  :demand
  :config
  (global-set-key (kbd "M-s-f") 'helm-do-grep-ag)
  (global-set-key (kbd "M-s-ƒ") 'helm-do-grep-ag))


(use-package hl-todo
  :demand
  :config (global-hl-todo-mode t))


(use-package magit
  :demand
  :config (global-magit-file-mode t))


(use-package multiple-cursors
  :demand
  :config (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines))


(use-package nix-mode
  :mode "\\.nix\\'")


(use-package smex
  :demand
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))


(use-package tuareg
  :mode ("\\.ml\\'" "\\.mli\\'"))


(use-package whitespace-cleanup-mode
  :demand
  :config (global-whitespace-cleanup-mode t))
