;; ===== PAREDIT =====

(require-package 'paredit)
(autoload 'enable-paredit-mode "paredit")

(require-package 'paredit-everywhere)
(add-hook 'prog-mode-hook 'paredit-everywhere-mode)
(add-hook 'css-mode-hook 'paredit-everywhere-mode)


(provide 'init-paredit)
