;;; init-javascript.el --- JS config

;;; Commentary:
;; Based on https://github.com/purcell/emacs.d/blob/8208151ab23cdcaa7b1027d16d8bd108a3b0dfd6/lisp/init-javascript.el

;;; Code:

(require 'init-elpa)
(require 'init-utils)

(require-package 'js2-mode)
(require-package 'js-comint)
(require-package 'json-mode)
(require-package 'jasminejs-mode)
(maybe-require-package 'ac-js2)

(require 'flycheck)
(require 'js2-mode)
(require 'js-comint)
(require 'jasminejs-mode)

(defcustom preferred-javascript-mode
  (first (remove-if-not #'fboundp '(js2-mode js-mode)))
  "Javascript mode to use for .js files."
  :type    'symbol
  :group   'programming
  :options '(js2-mode js-mode))

(defvar preferred-javascript-indent-level 2)

;; Need to first remove from list if present, since elpa adds entries too,
;; which may be in an arbitrary order

(eval-when-compile (require 'cl))

(setq auto-mode-alist
      (cons `("\\.js\\(\\.erb\\)?\\'" . ,preferred-javascript-mode)
            (loop for entry in auto-mode-alist
                  unless (eq preferred-javascript-mode (cdr entry))
                  collect entry)))

;; js2-mode
(after-load 'js2-mode
  ;; Disable js2 mode's syntax error highlighting by default...
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)
  ;; ... but enable it if flycheck can't handle javascript
  (autoload 'flycheck-get-checker-for-buffer "flycheck")
  (defun sanityinc/disable-js2-checks-if-flycheck-active ()
    (unless (flycheck-get-checker-for-buffer)
      (set (make-local-variable 'js2-mode-show-parse-errors) t)
      (set (make-local-variable 'js2-mode-show-strict-warnings) t)))
  (add-hook 'js2-mode-hook 'sanityinc/disable-js2-checks-if-flycheck-active)

  (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))

  (add-hook 'js2-mode-hook (lambda () (jasminejs-mode)))

  (defvar jasmine-buffer "*jasmine-node-specs-buffer*")

  (defun jasmine-compile ()
    "Run Jasmine-Node"
    (interactive)
    (shell-command (concat "jasmine-node " buffer-file-name)
                   (get-buffer-create jasmine-buffer))
    (display-buffer jasmine-buffer)
    (with-current-buffer jasmine-buffer
      (ansi-color-apply-on-region (point-min) (point-max))))

  ;; It's 2015. Ain't nobody got time for semicolons.
  (setq-default js2-strict-missing-semi-warning nil)

  (setq-default js2-basic-offset    preferred-javascript-indent-level
                js2-bounce-indent-p nil)

  (js2-imenu-extras-setup))

;; js-mode
(setq-default js-indent-level preferred-javascript-indent-level)


(add-to-list 'interpreter-mode-alist (cons "node" preferred-javascript-mode))


(require-package 'rainbow-delimiters)
(dolist (hook '(js2-mode-hook js-mode-hook json-mode-hook))
  (add-hook hook 'rainbow-delimiters-mode))


;; ===== INFERIOR JS =====

(setq inferior-js-program-command "js")

(defvar inferior-js-minor-mode-map (make-sparse-keymap))
(define-key inferior-js-minor-mode-map "\C-x\C-e" 'js-send-last-sexp)
(define-key inferior-js-minor-mode-map "\C-\M-x" 'js-send-last-sexp-and-go)
(define-key inferior-js-minor-mode-map "\C-cb" 'js-send-buffer)
(define-key inferior-js-minor-mode-map "\C-c\C-b" 'js-send-buffer-and-go)
(define-key inferior-js-minor-mode-map "\C-cl" 'js-load-file-and-go)

(define-minor-mode inferior-js-keys-mode
  "Bindings for communicating with an inferior js interpreter."
  nil " InfJS" inferior-js-minor-mode-map)

(dolist (hook '(js2-mode-hook js-mode-hook))
  (add-hook hook 'inferior-js-keys-mode))

(provide 'init-javascript)
;;; init-javascript.el ends here
