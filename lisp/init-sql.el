(defun pg-format-region (beg end)
  "Format PostgreSQL in region between BEG and END."
  (interactive "r")
  (save-excursion
    (shell-command-on-region beg end "pg_format -s 2 -" nil t)))

(defun sql-format-region (beg end)
  "Format SQL in region between BEG and END."
  (interactive "r")
  (save-excursion
    (shell-command-on-region beg end "sqlformat -r -" nil t)))

(global-set-key (kbd "C-c M-f") 'sql-format-region)
