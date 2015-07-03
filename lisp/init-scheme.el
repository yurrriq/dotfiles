(add-hook 'scheme-mode-hook 'auto-complete-mode)
(add-hook 'scheme-mode-hook 'hs-minor-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook
          (lambda ()
            (put 'register-template 'scheme-indent-function 'defun)
            (put 'call-template 'scheme-indent-function 4)))

(require-package 'geiser)

(setq geiser-active-implementations '(guile))
;; (setq geiser-repl-startup-time 10000)
(setq geiser-repl-history-filename "~/.emacs.d/geiser-history")
(setq geiser-repl-query-on-kill-p nil)
(setq geiser-implementations-alist
      '(((dir "~/src/yurrriq/exercism/scheme")
         guile)
        ((regexp "\\.scm$")
         guile)
        ((regexp "\\.ss$")
         racket)
        ((regexp "\\.rkt$")
         racket)))

(global-set-key (kbd "C-c M-g") 'run-geiser)

(provide 'init-scheme)
