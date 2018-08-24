(require 'lux-mode)

(add-hook 'lux-mode-hook #'paredit-mode)
(add-hook 'lux-mode-hook #'rainbow-delimiters-mode)
