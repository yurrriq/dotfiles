(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-utils)
(require 'init-elpa)
(require 'init-exec-path)

(require 'init-themes)
(require 'init-gui-frames)
(require 'init-dired)
(require 'init-flycheck)
(require 'init-windows)
(require 'init-sessions)
(require 'init-fonts)
;; (require 'init-mmm)
(require 'init-multi-web-mode)

(require 'init-editing-utils)
(require 'init-paredit)
(require 'init-lisp)
(require 'init-quicklisp)

(require 'init-csv)
(require 'init-css)
(require 'init-html)
(require 'init-markdown)

(require 'init-org)

(require 'init-clojure)
(require 'init-erlang)
(require 'init-frege)
(require 'init-go)
(require 'init-haskell)
(require 'init-javascript)
(require 'init-lfe)
(require 'init-lilypond)
(require 'init-scheme)
(require 'init-shen)

(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-x C-z"))

;; (global-set-key (kbd "s-r") 'helm-imenu-anywhere)
;; (global-set-key (kbd "A-r") 'helm-imenu-anywhere)


;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'text-mode-hook '(lambda() (set-fill-column 80)))

(autoload 'dash-at-point "dash-at-point"
  "Search the word at point with Dash." t nil)
(global-set-key "\C-cd" 'dash-at-point)
(global-set-key "\C-ce" 'dash-at-point-with-docset)

(setq inferior-lisp-program (executable-find "sbcl"))

;; (setq lisp-indent-offset 2)

;; (put 'add-hook 'lisp-indent-function 1)

;; (setq load-path (append (list "/usr/local/Cellar/lilypond/2.18.2_3/share/emacs/site-lisp/")) load-path)

(add-to-list 'load-path "~/site-lisp")
(autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.ily$" . LilyPond-mode))
(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))

(defun pg-format-region (beg end)
  "Format PostgreSQL in region between beg and end."
  (interactive "r")
  (save-excursion
    (shell-command-on-region beg end "pg_format -s 2 -" nil t)))

(defun sql-format-region (beg end)
  "Format SQL in region between beg and end."
  (interactive "r")
  (save-excursion
    (shell-command-on-region beg end "sqlformat -r -" nil t)))

(global-set-key (kbd "C-c M-f") 'sql-format-region)

;; (put 'after-load 'lisp-indent-function 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(edts-man-root "/Users/yurrriq/.emacs.d/edts/doc/17.5")
 '(safe-local-variable-values
   (quote
    ((org-export-html-style . "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/styles.css\" />")
     (org-export-html-style . "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/style.css\">")
     (org-html-doctype . "html5")
     (css-indent-offset . 2))))
 '(session-use-package t nil (session)))
