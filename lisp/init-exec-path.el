;; Based on = https://github.com/purcell/emacs.d/blob/8208151ab23cdcaa7b1027d16d8bd108a3b0dfd6/lisp/init-exec-path.el

(require-package 'exec-path-from-shell)

(after-load 'exec-path-from-shell
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var))
  ;; https://github.com/yurrriq/kerl
  (let ((path (shell-command-to-string "source ~/src/erlang/17.5/activate.fish; echo -n $PATH")))
    (setenv "PATH" path)
    (setq exec-path (append (split-string-and-unquote path " ") exec-path))))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(provide 'init-exec-path)
