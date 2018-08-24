;;; init-dired.el --- dired config

;;; Commentary:
;; Based on https://github.com/purcell/emacs.d/blob/8208151ab23cdcaa7b1027d16d8bd108a3b0dfd6/lisp/init-dired.el

;;; Code:

(require-package 'dired+)

(require-package 'dired-sort)

(setq-default diredp-hide-details-initially-flag nil dired-dwim-target t)

(after-load 'dired
  (require 'dired+)
  (require 'dired-sort)
  (when (fboundp 'global-dired-hide-details-mode)
    (global-dired-hide-details-mode -1))
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file))

(when (maybe-require-package 'diff-hl)
  (after-load 'dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

;; ===== GNU LS =====

(let ((gls "/usr/local/bin/gls"))
  (when (file-exists-p gls)
    (setq insert-directory-program gls)))


(provide 'init-dired)
;;; init-dired.el ends here
