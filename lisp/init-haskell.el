;; Based on https://github.com/purcell/emacs.d/blob/8208151ab23cdcaa7b1027d16d8bd108a3b0dfd6/lisp/init-haskell.el

(require-package 'haskell-mode)


;; ===== COMPLETION =====
;; Hook auto-complete into the completions provided by the inferior
;; haskell process, if any.


(require-package 'ac-haskell-process)

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

    (require 'flycheck-hdevtools)))

(defun unicode-symbol (name)
  "Translate a symbolic name for a Unicode character -- e.g., LEFT-ARROW
  or GREATER-THAN into an actual Unicode character code. "
  (decode-char 'ucs (case name
		      (left-arrow 8592)
		      (up-arrow 8593)
		      (right-arrow 8594)
		      (down-arrow 8595)
		      (double-vertical-bar #X2551)
		      (equal #X003d)
		      (not-equal #X2260)
		      (identical #X2261)
		      (not-identical #X2262)
		      (less-than #X003c)
		      (greater-than #X003e)
		      (less-than-or-equal-to #X2264)
		      (greater-than-or-equal-to #X2265)
		      (logical-and #X2227)
		      (logical-or #X2228)
		      (logical-neg #X00AC)
		      ('nil #X2205)
		      (horizontal-ellipsis #X2026)
		      (double-exclamation #X203C)
		      (prime #X2032)
		      (double-prime #X2033)
		      (for-all #X2200)
		      (there-exists #X2203)
		      (element-of #X2208)
		      (square-root #X221A)
		      (squared #X00B2)
		      (cubed #X00B3)
		      (lambda #X03BB)
		      (alpha #X03B1)
		      (beta #X03B2)
		      (gamma #X03B3)
		      (delta #X03B4))))

(defun substitute-pattern-with-unicode (pattern symbol)
  "Add a font lock hook to replace the matched part of PATTERN with the
  Unicode symbol SYMBOL looked up with UNICODE-SYMBOL."
  (font-lock-add-keywords
   nil `((,pattern
	  (0 (progn (compose-region (match-beginning 1) (match-end 1)
				    ,(unicode-symbol symbol)
				    'decompose-region)
		    nil))))))

(defun substitute-patterns-with-unicode (patterns)
  "Call SUBSTITUTE-PATTERN-WITH-UNICODE repeatedly."
  (mapcar #'(lambda (x)
	      (substitute-pattern-with-unicode (car x)
					       (cdr x)))
	  patterns))

(defun haskell-unicode ()
  ;; (interactive)
  (substitute-patterns-with-unicode
   (list (cons "\\s \\(<-\\)\\s " 'left-arrow)
     (cons "\\s \\(->\\)\\s " 'right-arrow)
     (cons "\\s \\(==\\)\\s " 'identical)
     (cons "\\s \\(/=\\)\\s " 'not-identical)
     (cons "\\s \\(()\\)\\(\\s \\|$\\)" 'nil)
     (cons "\\<\\(sqrt\\)\\>" 'square-root)
     (cons "\\s \\(&&\\)\\s " 'logical-and)
     (cons "\\s \\(||\\)\\s " 'logical-or)
     (cons "\\<\\(not\\)\\>" 'logical-neg)
     (cons "\\s \\(>\\)\\[^=\\]" 'greater-than)
     (cons "\\s \\(<\\)\\[^=\\]" 'less-than)
     (cons "\\s \\(>=\\)\\s " 'greater-than-or-equal-to)
     (cons "\\s \\(<=\\)\\s " 'less-than-or-equal-to)
     (cons "\\<\\(alpha\\)\\>" 'alpha)
     (cons "\\<\\(beta\\)\\>" 'beta)
     (cons "\\<\\(gamma\\)\\>" 'gamma)
     (cons "\\<\\(delta\\)\\>" 'delta)
     (cons "\\s \\(''\\)\\s " 'double-prime)
     (cons "\\s \\('\\)\\s " 'prime)
     (cons "\\s (?\\(\\\\\\)\\s *\\(\\w\\|_\\).*?\\s *->" 'lambda)
     (cons "\\s \\(!!\\)\\s " 'double-exclamation)
    ;; (cons "\\s \\(\\.\\.\\)s " 'horizontal-ellipsis)
    (cons "\\(\\.\\.\\)" 'horizontal-ellipsis))))

(defun prettify-haskell-symbols ()
  (setq prettify-symbols-alist
	'(("\\" . 955)     ; λ
	  ("<-" . 8592)    ; ←
	  ("->" . 8594)    ; →
	  ("-->" . 10230)  ; ⟶
	  ("=>" . 8658)    ; ⇒
	  ("map" . 8614))) ; ↦
  (prettify-symbols-mode))

(add-hook 'haskell-mode-hook 'haskell-unicode)
(add-hook 'haskell-mode-hook 'turn-on-eldoc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc)
(add-hook 'haskell-mode-hook 'prettify-haskell-symbols)


;; ===== DOCS =====

(dolist (hook '(haskell-mode-hook inferior-haskell-mode-hook haskell-interactive-mode-hook))
  (add-hook hook 'turn-on-haskell-doc-mode)
  (add-hook hook (lambda () (subword-mode +1)))
  (add-hook hook (lambda () (eldoc-mode 1))))

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; (add-hook 'haskell-interactive-mode-hook 'sanityinc/no-trailing-whitespace)


;; ===== INTERACTION =====

(after-load 'haskell
  (diminish 'interactive-haskell-mode " IntHS"))

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


(provide 'init-haskell)
