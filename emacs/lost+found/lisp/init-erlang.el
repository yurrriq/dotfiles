;;; init-erlang.el --- Customizations for writing Erlang

;;; Commentary:

;;; Code:

(require-package 'erlang)

(require 'erlang-start)
;; (require 'edts-start)

(after-load 'erlang
  (add-to-list 'auto-mode-alist '("\\.\\(e\\|h\\)rl?$"         . erlang-mode))
  (add-to-list 'auto-mode-alist '("\\(sys\\|rebar\\)\\.config" . erlang-mode))
  (add-to-list 'exec-path       "usr/local/lib/erlang/bin")
  (setq erlang-indent-level 2)
  (setq erlang-man-root-dir "/usr/local/lib/erlang/man")
  (setq erlang-root-dir     "/usr/local/lib/erlang"))

;; (add-to-list 'ac-modes 'erlang-mode)

(provide 'init-erlang)
;;; init-erlang.el ends here
