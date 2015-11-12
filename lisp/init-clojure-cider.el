(require 'init-clojure)
(require-package 'emacs '(24))

;; (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

(require-package 'cider)
(require-package 'ac-cider)


;; ===== AUTO COMPLETE =====

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))


;; ==== NREPL =====

(setq nrepl-popup-stacktraces nil)

(after-load 'cider
  (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
  (add-hook 'cider-mode-hook 'ac-cider-setup)
  (after-load 'auto-complete
    (add-to-list 'ac-modes 'cider-repl-mode))
  (add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)
  (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
  (add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  ;; (add-hook 'cider-repl-mode-hook 'subword-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook 'auto-complete-mode)
  (define-key cider-mode-map (kbd "C-c C-d") 'ac-cider-popup-doc)

  ;; nrepl isn't based on comint
  (add-hook 'cider-repl-mode-hook
            (lambda () (setq show-trailing-whitespace nil))))


;; ===== FLYCHECK =====

;; (eval-after-load 'flycheck '(flycheck-clojure-setup))
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; (eval-after-load 'flycheck
;;   '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))


;; ===== KIBIT =====

;; Teach compile the syntax of the kibit output
;; (add-to-list 'compilation-error-regexp-alist-alist
;;           '(kibit "At \\([^:]+\\):\\([[:digit:]]+\\):" 1 2 nil 0))
;; (add-to-list 'compilation-error-regexp-alist 'kibit)

;; (defun kibit ()
;;   "Runs kibit on the current project.
;;   Displays the results in a hyperlinked *compilation* buffer."
;;   (interactive)
;;   (compile "lein kibit"))

;; (defun kibit-current-file ()
;;   "Runs kibit on the current file.
;;    Displays the results in a hyperlinked *compilation* buffer."
;;   (interactive)
;;   (compile (concat "lein kibit " buffer-file-name)))


(provide 'init-clojure-cider)
