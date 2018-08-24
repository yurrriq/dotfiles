;;; init-lfe.el --- Config for programming Lisp Flavoured Erlang

;;; Commentary:

;;; Code:

(defvar lfe-dir (concat (getenv "HOME") "/src/rvirding/lfe/emacs"))

(setq load-path (cons lfe-dir load-path))

(require 'lfe-mode)
(require 'lfe-start)

(define-lfe-indent
  ;; defmodule
  (from   1)
  (import 0)

  ;; exemplar
  (body    0)
  (div     1)
  (head    0)
  (html    0)
  (link-to 1)
  (pre     1)
  (ul      0)

  ;; (main 0)
  ;; (list 0)

  ;; (GET 1)

  (lodox-util:when* 1)

  ;; lists
  (lists:map   0)
  (lists:foldl 0)
  (lists:sort  0))

(add-hook 'lfe-mode-hook 'paredit-mode)
(add-hook 'lfe-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lfe-mode-hook 'auto-complete-mode)

(provide 'init-lfe)
;;; init-lfe.el ends here
