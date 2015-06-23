(require-package 'unfill)

(when (fboundp 'electric-pair-mode)
  (electric-pair-mode))

(electric-indent-mode 1)

(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 fill-column 80
 indent-tabs-mode nil
 make-backup-files nil
 mouse-yank-at-point t
 ring-bell-function 'ignore
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 show-trailing-whitespace t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil)
;; visible-bell t

(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(transient-mark-mode t)


;; ===== WHITESPACE CLEANUP =====

(require-package 'whitespace-cleanup-mode)
(global-whitespace-cleanup-mode t)

(global-set-key [remap just-one-space] 'cycle-spacing)


;; ===== PRETTIFY SYMBOLS =====

(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))


;; ===== SHOW MATCHING PARENS =====
(show-paren-mode 1)


(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)


;; ===== FILL COLUMN INDICATOR =====

(when (eval-when-compile (> emacs-major-version 23))
  (require-package 'fill-column-indicator)
  (defun sanityinc/prog-mode-fci-settings ()
    (turn-on-fci-mode)
    (when show-trailing-whitespace
      (set (make-local-variable 'whitespace-style) '(face trailing))
      (whitespace-mode 1)))

  ;;(add-hook 'prog-mode-hook 'sanityinc/prog-mode-fci-settings)

  (defun sanityinc/fci-enabled-p ()
    (and (boundp 'fci-mode) fci-mode))

  (defvar sanityinc/fci-mode-suppressed nil)
  (defadvice popup-create (before suppress-fci-mode activate)
    "Suspend fci-mode while popups are visible"
    (let ((fci-enabled (sanityinc/fci-enabled-p)))
      (when fci-enabled
        (set (make-local-variable 'sanityinc/fci-mode-suppressed) fci-enabled)
        (turn-off-fci-mode))))
  (defadvice popup-delete (after restore-fci-mode activate)
    "Restore fci-mode when all popups have closed"
    (when (and sanityinc/fci-mode-suppressed
               (null popup-instances))
      (setq sanityinc/fci-mode-suppressed nil)
      (turn-on-fci-mode)))

  ;; Regenerate fci-mode line images after switching themes
  (defadvice enable-theme (after recompute-fci-face activate)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (sanityinc/fci-enabled-p)
          (turn-on-fci-mode))))))


;; ===== IDO GOTO SYMBOL ======

(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
 	  (ido-enable-flex-matching
 	   (if (boundp 'ido-enable-flex-matching)
 	       ido-enable-flex-matching t))
 	  name-and-pos symbol-names position)
      (unless ido-mode
 	(ido-mode 1)
 	(setq ido-enable-flex-matching t))
      (while (progn
 	       (imenu--cleanup)
 	       (setq imenu--index-alist nil)
 	       (ido-goto-symbol (imenu--make-index-alist))
 	       (setq selected-symbol
 		     (ido-completing-read "Symbol? " symbol-names))
 	       (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
 	(push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
 	(goto-char (overlay-start position)))
       (t
 	(goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
 	(cond
 	 ((and (listp symbol) (imenu--subalist-p symbol))
 	  (ido-goto-symbol symbol))
 	 ((listp symbol)
 	  (setq name (car symbol))
 	  (setq position (cdr symbol)))
 	 ((stringp symbol)
 	  (setq name symbol)
 	  (setq position
 		(get-text-property 1 'org-imenu-marker symbol))))
 	(unless (or (null position) (null name)
 		    (string= (car imenu--rescan-item) name))
 	  (add-to-list 'symbol-names name)
 	  (add-to-list 'name-and-pos (cons name position))))))))

(global-set-key (kbd "A-r") 'ido-goto-symbol)
(global-set-key (kbd "s-r") 'ido-goto-symbol)


;; ===== INDENTATION =====

(defun indent-entire-sexp ()
  (interactive)
  (let ((p1 (point)))
    (beginning-of-defun)
    (indent-pp-sexp)
    (goto-char p1)))

(global-set-key (kbd "s-i") 'indent-entire-sexp)
(global-set-key (kbd "A-i") 'indent-entire-sexp)

(defun indent-buffer ()
  (interactive)
  (let ((p1 (point)))
    (indent-region (point-min) (point-max))
    (goto-char p1)))

(global-set-key (kbd "M-s-^") 'indent-buffer)
(global-set-key (kbd "A-M-i") 'indent-buffer)


;; ===== COMMENTS =====

(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line
  if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)


;; ===== HELM AG =====

(require-package 'helm-ag)

(global-set-key (kbd "M-s-ƒ") 'helm-ag)
(global-set-key (kbd "M-A-f") 'helm-ag)

(global-set-key (kbd "C-c M-a") 'helm-ag)


;; ===== HIDE/SHOW =====

(require-package 'hideshow)

(global-set-key (kbd "C-M-h") 'hs-toggle-hiding)

;; (global-set-key (kbd "<f2> h h") 'hs-hide-all)
;; (global-set-key (kbd "<f2> h j") 'hs-show-all)

(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)


(provide 'init-editing-utils)
