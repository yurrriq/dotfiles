;; https://github.com/purcell/emacs.d/blob/8208151ab23cdcaa7b1027d16d8bd108a3b0dfd6/lisp/init-lisp.el

(require-package 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

(require-package 'lively)

(setq-default initial-scratch-message
              (concat ";; Happy hacking " (or user-login-name "") " - Emacs ♥ you!\n\n"))


;; Make C-x C-e run 'eval-region if the region is active

(defun sanityinc/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

(global-set-key (kbd "M-:") 'pp-eval-expression)

(after-load 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-x C-e") 'sanityinc/eval-last-sexp-or-region))


;; ;; https://github.com/steckerhalter/ipretty
;; (require-package 'ipretty)
;; (ipretty-mode 1)


(defadvice pp-display-expression (after make-read-only (expression out-buffer-name) activate)
  "Enable `view-mode' in the output buffer - if any - so it can be closed with `\"q\"."
  (when (get-buffer out-buffer-name)
    (with-current-buffer out-buffer-name
      (view-mode 1))))


;; ----------------------------------------------------------------------------
;; Hippie-expand
;; ----------------------------------------------------------------------------

;; (defun my/emacs-lisp-module-name ()
;;   "Search the buffer for `provide' declaration."
;;   (save-excursion
;;     (goto-char (point-min))
;;     (when (search-forward-regexp "^(provide '" nil t)
;;       (symbol-name (symbol-at-point)))))

;; ;; Credit to Chris Done for this one.
;; (defun my/try-complete-lisp-symbol-without-namespace (old)
;;   "Hippie expand \"try\" function which expands \"-foo\" to \"modname-foo\" in elisp."
;;   (unless old
;;     (he-init-string (he-lisp-symbol-beg) (point))
;;     (when (string-prefix-p "-" he-search-string)
;;       (let ((mod-name (my/emacs-lisp-module-name)))
;;         (when mod-name
;;           (setq he-expand-list (list (concat mod-name he-search-string)))))))
;;   (when he-expand-list
;;     (he-substitute-string (car he-expand-list))
;;     (setq he-expand-list nil)
;;     t))

;; (defun set-up-hippie-expand-for-elisp ()
;;   "Locally set `hippie-expand' completion functions for use with Emacs Lisp."
;;   (make-local-variable 'hippie-expand-try-functions-list)
;;   (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
;;   (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t)
;;   (add-to-list 'hippie-expand-try-functions-list 'my/try-complete-lisp-symbol-without-namespace t))


;; ----------------------------------------------------------------------------
;; Automatic byte compilation
;; ----------------------------------------------------------------------------

(require-package 'auto-compile)
(auto-compile-on-save-mode 1)
(auto-compile-on-load-mode 1)

;; ----------------------------------------------------------------------------
;; Load .el if newer than corresponding .elc
;; ----------------------------------------------------------------------------
(setq load-prefer-newer t)


(require-package 'rainbow-delimiters)

;; (require-package 'redshank)
;; (after-load 'redshank
;;   (diminish 'redshank-mode))

;; (maybe-require-package 'aggressive-indent)


(defun sanityinc/lisp-setup ()
  "Enable features useful in any Lisp mode."
  (rainbow-delimiters-mode t)
  (enable-paredit-mode)
  ;; (when (fboundp 'aggressive-indent-mode)
  ;;   (aggressive-indent-mode))
  (turn-on-eldoc-mode)
  ;; (redshank-mode)
  (add-hook 'after-save-hook #'check-parens nil t))


(defun sanityinc/emacs-lisp-setup ()
  "Enable features useful when working with elisp."
  (elisp-slime-nav-mode t)
  ;; (set-up-hippie-expand-for-elisp)
  ;; (ac-emacs-lisp-mode-setup)
  )


(defconst sanityinc/elispy-modes
  '(emacs-lisp-mode ielm-mode)
  "Major modes relating to elisp.")

(defconst sanityinc/lispy-modes
  (append sanityinc/elispy-modes
          '(lisp-mode inferior-lisp-mode lisp-interaction-mode))
  "All lispy major modes.")

(require 'derived)

(dolist (hook (mapcar #'derived-mode-hook-name sanityinc/lispy-modes))
  (add-hook hook 'sanityinc/lisp-setup))

(dolist (hook (mapcar #'derived-mode-hook-name sanityinc/elispy-modes))
  (add-hook hook 'sanityinc/emacs-lisp-setup))

(if (boundp 'eval-expression-minibuffer-setup-hook)
    (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  (require-package 'eldoc-eval)
  (require 'eldoc-eval)
  (eldoc-in-minibuffer-mode 1))

(add-to-list 'auto-mode-alist '("\\.emacs-project\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("archive-contents\\'" . emacs-lisp-mode))


(when (maybe-require-package 'rainbow-mode)
  (defun sanityinc/enable-rainbow-mode-if-theme ()
    (when (string-match "\\(color-theme-\\|-theme\\.el\\)" (buffer-name))
      (rainbow-mode 1)))
  (add-hook 'emacs-lisp-mode-hook 'sanityinc/enable-rainbow-mode-if-theme))

;; (when (maybe-require-package 'highlight-quoted)
;;   (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode))


(when (maybe-require-package 'flycheck)
  (require-package 'flycheck-package)
  (after-load 'flycheck
    (flycheck-package-setup)))

(after-load 'lisp-mode
  (require-package 'compile)

  ;; this means hitting the compile button always saves the buffer
  ;; having to separately hit C-x C-s is a waste of time
  (setq mode-compile-always-save-buffer-p t)
  ;; make the compile window stick at 12 lines tall
  ;; (setq compilation-window-height 12)

  ;; from enberg on #emacs
  ;; if the compilation has a zero exit code,
  ;; the windows disappears after two seconds
  ;; otherwise it stays
  ;; (setq compilation-finish-function
  ;;       (lambda (buf str)
  ;;         (unless (string-match "[1-9][0-9]* failed" str)
  ;;           ;;no errors, make the compilation window go away in a few seconds
  ;;           (run-at-time
  ;;            "2 sec" nil 'delete-windows-on
  ;;            (get-buffer-create "*compilation*"))
  ;;           (message "No Compilation Errors!"))))

  ;; one-button testing, tada!
  (global-set-key [f12] 'compile))

(provide 'init-lisp)
