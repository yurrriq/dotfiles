;;; init-lilypond --- Editing LilyPond files

;;; Commentary:

;;; Code:

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.i?ly$" . LilyPond-mode))
(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))

(provide 'init-lilypond)
;;; init-lilypond ends here
