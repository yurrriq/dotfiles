(require-package 'lfe-mode)

(defvar lfe-dir (concat (getenv "HOME") "/src/lfe/lfe/emacs"))
(setq load-path (cons lfe-dir load-path))
(require 'lfe-start)


(provide 'init-lfe)
