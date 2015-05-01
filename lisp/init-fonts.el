;; ===== DEFAULT FONT =====

(set-face-attribute 'default nil
                    :family "Source Code Pro for Powerline"
                    ;; :family "Office Code Pro"
                    :height 140
                    :weight 'regular
                    :width 'normal)


;; ===== CHANGING FONT SIZES =====

(require-package 'default-text-scale)
(global-set-key (kbd "C-M-=") 'default-text-scale-increase)
(global-set-key (kbd "C-M--") 'default-text-scale-decrease)


(provide 'init-fonts)
