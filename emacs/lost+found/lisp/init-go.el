(require-package 'go-mode)

(add-hook 'go-mode-hook
          (lambda ()
            (substitute-key-definition 'go-import-add 'helm-go-package go-mode-map)
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq-default)
            (setq tab-width 2)
            (setq standard-indent 2)
            (setq indent-tabs-mode nil)))

(defun go ()
  "run current buffer"
  (interactive)
  (compile (concat "go run \"" (buffer-file-name) "\"")))


(provide 'init-go)
