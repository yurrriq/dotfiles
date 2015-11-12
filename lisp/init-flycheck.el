;;; init-flycheck.el --- Flycheck config

;;; Commentary:
;; https://github.com/purcell/emacs.d/blob/8208151ab23cdcaa7b1027d16d8bd108a3b0dfd6/lisp/init-flycheck.el

;;; Code:

(require 'init-elpa)

(require-package 'flycheck)

(require 'flycheck)

(setq flycheck-emacs-lisp-load-path 'inherit)

(setq flycheck-idle-change-delay    0.8)

;; Override default flycheck triggers
(setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
      flycheck-idle-change-delay 0.8)

;; (eval-after-load 'flycheck '(flycheck-clojure-setup))

(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

(add-hook 'after-init-hook 'global-flycheck-mode)

(provide 'init-flycheck)
;;; init-flycheck.el ends here
