;;; init-haskell --- Haskell config

;;; Commentary:
;; Based on https://github.com/purcell/emacs.d/blob/8208151ab23cdcaa7b1027d16d8bd108a3b0dfd6/lisp/init-haskell.el

;;; Code:

(require-package 'haskell-mode)


;; ===== COMPLETION =====
;; Hook auto-complete into the completions provided by the inferior
;; haskell process, if any.

(require 'haskell-font-lock)

(require-package 'ac-haskell-process)
(require 'ac-haskell-process)

(add-hook 'interactive-haskell-mode-hook 'ac-haskell-process-setup)
(add-hook 'haskell-interactive-mode-hook 'ac-haskell-process-setup)

(after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c C-d") 'ac-haskell-process-popup-doc))

(after-load 'auto-complete
  (add-to-list 'ac-modes 'haskell-interactive-mode)
  (add-hook 'haskell-interactive-mode-hook 'set-auto-complete-as-completion-at-point-function))


;; ===== FLYCHECK =====

(when (and (maybe-require-package 'flycheck-haskell)
           (require-package 'flycheck-hdevtools))
  (after-load 'flycheck
    (add-hook 'haskell-mode-hook #'flycheck-haskell-setup)

    (defun sanityinc/flycheck-haskell-reconfigure ()
      "Reconfigure flycheck haskell settings, e.g. after changing cabal file."
      (interactive)
      (unless (eq major-mode 'haskell-mode)
        (error "Expected to be in haskell-mode"))
      (flycheck-haskell-clear-config-cache)
      (flycheck-haskell-configure)
      (flycheck-mode -1)
      (flycheck-mode))
    (defadvice haskell-mode-stylish-buffer (around skip-if-flycheck-errors activate)
      "Don't run stylish-buffer if the buffer appears to have a syntax error.
      This isn't a hard guarantee, since flycheck might sometimes not run until the file has
      been saved."
      (unless (flycheck-has-current-errors-p 'error)
        ad-do-it))

    ;; (require 'flycheck-hdevtools)
    ))

(dolist (hook '(eldoc-mode turn-on-haskell-font-lock interactive-haskell-mode))
  (add-hook 'haskell-mode-hook hook))


;; ===== DOCS =====

(dolist (hook '(haskell-mode-hook inferior-haskell-mode-hook haskell-interactive-mode-hook))
  (add-hook hook 'turn-on-haskell-doc)
  (add-hook hook (lambda () (subword-mode +1)))
  (add-hook hook (lambda () (eldoc-mode 1))))

;; (add-hook 'haskell-interactive-mode-hook 'sanityinc/no-trailing-whitespace)


;; ===== INTERACTION =====

(after-load 'haskell (diminish 'interactive-haskell-mode " IntHS"))

(add-auto-mode 'haskell-mode "\\.ghci\\'")

(when (maybe-require-package 'ghci-completion)
  (add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion))


;; ===== INDENTATION =====

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)


;; ===== SOURCE CODE HELPERS =====

(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

(setq-default haskell-stylish-on-save t)

(maybe-require-package 'hayoo)
(after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c h") 'hoogle)
  (define-key haskell-mode-map (kbd "C-o") 'open-line)
  (require 'compile)

  ;; this means hitting the compile button always saves the buffer
  ;; having to separately hit C-x C-s is a waste of time
  (setq mode-compile-always-save-buffer-p t)
  ;; make the compile window stick at 12 lines tall
  (setq compilation-window-height 12)

  ;; from enberg on #emacs
  ;; if the compilation has a zero exit code,
  ;; the windows disappears after two seconds
  ;; otherwise it stays
  ;; (setq compilation-finish-function
  ;;       (lambda (buf str)
  ;;         (unless (string-match "exited abnormally" str)
  ;;           ;;no errors, make the compilation window go away in a few seconds
  ;;           (run-at-time
  ;;            "2 sec" nil 'delete-windows-on
  ;;            (get-buffer-create "*compilation*"))
  ;;           (message "No Compilation Errors!"))))

  ;; one-button testing, tada!
  (global-set-key [f12] 'compile))


(after-load 'page-break-lines
  (push 'haskell-mode page-break-lines-modes))


;; ===== COMPILATION =====
;; Make compilation-mode understand "at blah.hs:11:34-50" lines output by GHC

(after-load 'compile
  (let ((alias 'ghc-at-regexp))
    (add-to-list
     'compilation-error-regexp-alist-alist
     (list alias " at \\(.*\\.\\(?:l?[gh]hs\\|hi\\)\\):\\([0-9]+\\):\\([0-9]+\\)-[0-9]+$" 1 2 3 0 1))
    (add-to-list
     'compilation-error-regexp-alist alias)))

;; ===== STACK MODE =====

;; (add-to-list 'load-path "~/src/commercialhaskell/stack-ide/stack-mode/")
;; (require 'stack-mode)
;; (add-hook 'haskell-mode-hook 'stack-mode)


(provide 'init-haskell)
;;; init-haskell.el ends here
