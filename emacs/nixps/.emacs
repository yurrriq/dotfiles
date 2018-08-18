(server-start)
(menu-bar-mode 0)
(tool-bar-mode 0)

(require 'package)

(setq package-archives nil)
(setq package-enable-at-startup nil)

(setq load-path
      (append (reverse (mapcar (lambda (x) (concat x "/share/emacs/site-lisp/"))
			       (split-string (or (getenv "NIX_PROFILES") ""))))
	      load-path))

(package-initialize)

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


(setq c-default-style "k&r"
      c-basic-offset 4)


(require 'clojure-mode)

(dolist (hook '(fci-mode paredit-mode rainbow-delimiters-mode))
  (add-to-list 'clojure-mode-hook hook)
  (add-to-list 'emacs-lisp-mode-hook hook))


(setq js-indent-level 2)


(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)))


(setq text-mode-hook '(text-mode-hook-identify))


(global-whitespace-cleanup-mode t)


(global-set-key (kbd "C-a") 'crux-move-beginning-of-line)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C-x C-k") 'kill-this-buffer)

(global-set-key (kbd "M-s-f") 'helm-do-grep-ag)
