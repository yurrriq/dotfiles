;;; org-doing.el --- Keep track of what you're doing

;; Copyright (C) 2014 Rudolf Olah <omouse@gmail.com>

;; Author: Rudolf Olah
;; URL: https://github.com/omouse/org-doing
;; Version: 0.1
;; Created: 2014-03-16
;; By: Rudolf Olah
;; keywords: tools, org

;; org-doing is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; org-doing is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with org-doing. If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'cl)
(require 'org)

(provide 'org-doing)

(defgroup org-doing nil
  "Customization of org-doing"
  :version "0.1"
  :group :org)

(defcustom org-doing-file "~/doing.org"
  "The file where org-doing stores what you're doing now and later."
  :type '(string)
  :group 'org-doing)

(defun org-doing-find-or-create-file ()
  "Opens the `org-doing-file', if it doesn't exist, creates it.

If it exists, goes to the beginning of the file."
  (find-file org-doing-file)
  (if (file-exists-p org-doing-file)
      (goto-char (point-min))
    (insert "#+TITLE: doing\n"
            "#+STARTUP: overview\n"
            "#+TODO: TODO LATER | DONE\n\n")))

;;;###autoload
(defun org-doing-log (description &optional later-p)
  "Logs the `description' of what you're doing now in the file
`org-doing-file' at the *top* of the file.

When `later-p' is true, logs the item as something to be done
later."

  (interactive "sDoing? \nP")

  (org-doing-find-or-create-file)
  (if (search-forward-regexp "^* " nil t)
      (beginning-of-line)
    (goto-char (point-max)))
  (insert "* " (if later-p "LATER" "TODO") " " description "\n"
          "  " (format-time-string "<%Y-%m-%d %a %H:%M>\n"))
  (save-buffer))

(defun org-doing-done (description)
  "Inserts a new heading into `org-doing-file' that's marked as DONE.

If `description' is nil or a blank string, marks the most recent
TODO item as DONE (see `org-doing-done-most-recent-item'.)"
  (interactive "sDone? ")
  (org-doing-find-or-create-file)
  (if (zerop (length description))
      (org-doing-done-most-recent-item)
    (if (search-forward-regexp "^* " nil t)
        (beginning-of-line)
      (goto-char (point-max)))
    (insert "* DONE " description "\n"
            "  " (format-time-string "<%Y-%m-%d %a %H:%M>\n")))
  (save-buffer))

(defun org-doing-done-most-recent-item ()
  "Marks the most recent item in `org-doing-file' as DONE."
  (if (search-forward-regexp "^* TODO" nil t)
    (replace-match "* DONE")))

;;;###autoload
(defun org-doing (command)
  "Interactive function for running any org-doing command.

The first part of the `command' string is parsed as a command:
- now: calls `org-doing-log'
- later: calls `org-doing-log'
- done: calls `org-doing-done'
"
  (interactive "sDoing? ")
  (let* ((first-space (search " " command))
         (cmd (downcase (subseq command 0 first-space)))
         (args (subseq command (+ first-space 1))))
    (cond ((string= cmd "now") (org-doing-log args))
          ((string= cmd "later") (org-doing-log args t))
          ((string= cmd "done") (org-doing-done args)))))

(provide 'org-doing)
;;; org-doing.el ends here
