(deftheme yurrriq
  "Created 2015-04-14.")

(custom-theme-set-variables
 'yurrriq
 '(ansi-color-faces-vector [default default default italic underline success warning error])
 '(aquamacs-customization-version-id 0)
 '(custom-safe-themes (quote ("a4e070d3db3ad091f72938be8e9f3b66e2dabd53c97a57071c720b394993c75f" "31a01668c84d03862a970c471edbd377b2430868eccf5e8a9aec6831f1a0908d" "4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" default)))
 '(edts-man-root "/Users/yurrriq/.emacs.d/edts/doc/17.5")
 '(haskell-mode-hook (quote (turn-on-eldoc-mode turn-on-haskell-doc)))
 '(tabbar-background-color "#353535")
 '(one-buffer-one-frame-mode nil)
 '(tabbar-mode t)
 '(session-use-package t))

(custom-theme-set-faces
 'yurrriq
 '(messages-buffer-mode-default ((t (:inherit special-mode-default))))
 '(special-mode-default ((t (:inherit autoface-default))))
 '(default ((t (:height 140 :family "Source Code Pro for Powerline"))))
 '(text-mode-default ((t (:inherit autoface-default :stipple nil :strike-through nil :underline nil :slant normal :weight normal :height 130 :width normal :family "Lucida Grande"))))
 '(minibuffer-inactive-mode-default ((t (:inherit autoface-default))))
 '(custom-theme-choose-mode-default ((t (:inherit special-mode-default))))
 '(fundamental-mode-default ((t (:inherit autoface-default))))
 '(prog-mode-default ((t (:inherit autoface-default))))
 '(emacs-lisp-mode-default ((t (:inherit prog-mode-default))))
 '(custom-new-theme-mode-default ((t (:inherit autoface-default)))))

(provide-theme 'yurrriq)
