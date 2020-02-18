(server-start)
(setq server-window 'pop-to-buffer-same-window)
(column-number-mode 1)
(add-to-list 'exec-path "/run/current-system/sw/bin")
(menu-bar-mode 0)

(tool-bar-mode 0)
(let ((font (if (eq system-type 'darwin)
                "Iosevka-16"
              "Iosevka-11")))
  (set-face-attribute 'default t :font font)
  (set-frame-font font nil t))
(require 'package)
(setq-default indent-tabs-mode nil
              package-archives nil
              package-enable-at-startup nil)
(package-initialize)

(eval-when-compile
  (require 'use-package))

(setq-default use-package-always-defer t
              use-package-always-ensure t)
(let ((backup-directory (concat user-emacs-directory "backup")))
  (unless (file-exists-p backup-directory)
    (make-directory backup-directory t))
  (setq backup-directory-alist `(("" . ,backup-directory))))

(setq auto-save-default         t
      auto-save-interval        200
      auto-save-timeout         20
      backup-by-copying         t
      delete-by-moving-to-trash t
      delete-old-versions       t
      kept-new-versions         6
      kept-old-versions         2
      make-backup-files         t
      vc-make-backup-files      t
      version-control           t)

(define-minor-mode sensitive-mode
  "For sensitive files like password lists.
It disables backup creation and auto saving.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " 🔒"
  ;; The minor mode bindings.
  nil
  (if (symbol-value sensitive-mode)
      (progn
        ;; disable backups
        (set (make-local-variable 'backup-inhibited) t)
        ;; disable auto-save
        (if auto-save-default
            (auto-save-mode -1)))
    ;; resort to default value of backup-inhibited
    (kill-local-variable 'backup-inhibited)
    ;; resort to default auto save setting
    (if auto-save-default
        (auto-save-mode 1))))
(dolist (pattern '("^\\(/dev/shm/\\|/tmp/\\)"
                   "\\.\\(enc\\|gpg\\|hashedPassword\\)$"))
  (add-to-list 'auto-mode-alist (cons pattern 'sensitive-mode)))

(setq custom-file "~/.emacs.d/private/local/custom.el")
(load-theme 'wombat)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "s-u") 'revert-buffer)
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
(use-package avy
  :demand
  :config
  (global-set-key (kbd "C-;") 'avy-goto-char)
  (global-set-key (kbd "C-'") 'avy-goto-char-2)
  (global-set-key (kbd "M-g f") 'avy-goto-line))
(use-package crux
  :demand
  :config (global-set-key (kbd "C-a") 'crux-move-beginning-of-line))
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))
(use-package emojify-mode
  :demand
  :config
  (global-emojify-mode)
  (global-emojify-mode-line-mode))
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
  :mode ("\\.nix\\'"))
(use-package noweb-mode
  :load-path "/run/current-system/sw/share/emacs/site-lisp"
  :mode ("\\.nw\\'")
  :demand)
(use-package robe
  :demand
  :mode ("\\.rb\\'"))
(use-package rvm
  :ensure t)
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
