;;; init.el --- Emacs config

;;; Commentary:

;;; Code:

;; (package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-elpa)
(require 'init-utils)

(require 'init-dash-at-point)
;; (require 'init-exec-path)
(require 'init-paradox)

(require 'init-themes)
(require 'init-gui-frames)
(require 'init-dired)
(require 'init-flycheck)
(require 'init-windows)
;; (require 'init-sessions)
(require 'init-fonts)
;; (require 'init-mmm)
(require 'init-multi-web-mode)

(require 'init-editing-utils)
(require 'init-origami)
(require 'init-paredit)
(require 'init-lisp)
;; (require 'init-quicklisp)

(require 'init-csv)
(require 'init-css)
(require 'init-html)
(require 'init-markdown)
(require 'init-org)

(require 'init-magit)

(require 'init-clojure)
(require 'init-clojure-cider)
(require 'init-erlang)
(require 'init-frege)
(require 'init-go)
(require 'init-haskell)
(require 'init-javascript)
(require 'init-lfe)
(require 'init-lilypond)
;; (require 'init-lux)
;; (require 'init-purescript)
(require 'init-scheme)
(require 'init-shen)
(require 'init-swift)

(require-package 'nyan-mode)
(nyan-mode)

(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-x C-z"))

;; (global-set-key (kbd "s-r") 'helm-imenu-anywhere)
;; (global-set-key (kbd "A-r") 'helm-imenu-anywhere)

(require 'ido)
(ido-mode 'buffers)
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
                           "*Messages*" "Async Shell Command"))

;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'text-mode-hook '(lambda() (set-fill-column 80)))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; init.el ends here
