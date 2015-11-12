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

;; ⌘⏎ ⇒ Toggle fullscreen. Like clicking the green circle.
(global-set-key (kbd "<s-return>") 'toggle-frame-fullscreen)

;; ⌥⏎ ⇒ Toggle maximization state of the selected frame.
(global-set-key (kbd "M-RET") 'toggle-frame-maximized)

;; ⌘ left click ⇒ Open the URL at or before the point in a browser.
(global-set-key (kbd "<s-mouse-1>") 'browse-url-at-point)

;; multiple-cursors.el --- Multiple cursors for emacs.
;; https://github.com/magnars/multiple-cursors.el
(require-package 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; http://stackoverflow.com/a/25212377/1793234
;; http://stackoverflow.com/a/384346/1793234
;; http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Rename current buffer and file it is visiting to `NEW-NAME'."
  (interactive "FNew name: ")           ; F -- Possibly nonexistent file name.
  (let ((name     (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      ;; let ((new-name (read-file-name "New name: " filename)))
      (if (get-buffer new-name)
          (error "A buffer named '%s' already exists!" new-name)
        (rename-file filename new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)
        (message "mv '%s' '%s'" name (file-name-nondirectory new-name))))))

(global-set-key (kbd "C-x C-r") 'rename-file-and-buffer)

(provide 'init-utils)
;;; init-utils.el ends here
