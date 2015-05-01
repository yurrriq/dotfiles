(add-hook 'scheme-mode-hook 'auto-complete-mode)
(add-hook 'scheme-mode-hook 'hs-minor-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook
          (lambda ()
            (put 'register-template 'scheme-indent-function 'defun)
            (put 'call-template 'scheme-indent-function 4)))

(provide 'init-scheme)
