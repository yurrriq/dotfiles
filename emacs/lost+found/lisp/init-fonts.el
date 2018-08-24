;;; init-fonts.el --- Default font setup

;;; Commentary:

;;; Code:

(set-face-attribute
 'default nil                ; set default face on all current and future frames
 :family  "Hack"             ; http://sourcefoundry.org/hack/
 :height  140                ; 14pt
 :weight 'normal
 :width  'normal)

(require-package 'default-text-scale)
(global-set-key (kbd "C-M-=") 'default-text-scale-increase)
(global-set-key (kbd "C-M--") 'default-text-scale-decrease)

(provide 'init-fonts)
;;; init-fonts.el ends here
