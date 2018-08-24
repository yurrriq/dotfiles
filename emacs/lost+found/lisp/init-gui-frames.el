(setq inhibit-startup-screen t)
;; (setq ns-pop-up-frames nil)

(setq indicate-empty-lines t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(global-set-key (kbd "M-ƒ") 'toggle-frame-fullscreen)

(setq-default fill-column 80)


(provide 'init-gui-frames)
