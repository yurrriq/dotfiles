;;; init-utils --- Global utility functions

;;; Commentary:
;; Based on https://github.com/purcell/emacs.d/blob/c60299cfdd799ccf81eefacb1a6fca1d9d703ff4/lisp/init-utils.el

;;; Code:

(declare-function org-babel-tangle "ob-tangle")

(defun yurrriq/auto-tangle-literate-config ()
  "Upon saving ~/.emacs.d/README.org, tangle it."
  (when (file-equal-p buffer-file-name "~/.emacs.d/README.org")
    (org-babel-tangle)))

(add-hook 'after-save-hook 'yurrriq/auto-tangle-literate-config)

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'init-utils)
;;; init-utils.el ends here
