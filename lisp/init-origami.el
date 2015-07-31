;;; init-origami --- origami-mode customizations

;;; Commentary:

;;; Code:

(require-package 'origami)

(require 'origami)

;; after-load 'origami-mode
(defun yurrriq/define-origami-keys (pairs)
  "For each (key . sym)' in `PAIRS', call `define-key' `origami-mode-map'."
  (mapcar #'(lambda (x) (define-key origami-mode-map (kbd (car x)) (cdr x))) pairs))
(yurrriq/define-origami-keys
 `(("<C-tab>"   . origami-toggle-node)
   ("<C-M-tab>" . origami-toggle-all-nodes)
   ("C-c a"     . origami-close-all-nodes)
   ("C-c s"     . origami-open-all-nodes)))
(global-origami-mode)

(provide 'init-origami)
;;; init-origami.el ends here
