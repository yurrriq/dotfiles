;;; init-org --- Configuration for org-mode

;;; Commentary:

;; Based on https://github.com/purcell/emacs.d/blob/8208151ab23cdcaa7b1027d16d8bd108a3b0dfd6/lisp/init-org.el


;;; Code:

(require 'init-elpa)
(require 'init-utils)

(require-package 'org-plus-contrib)
(require 'org)


(require-package 'org-doing)
(require 'org-doing)
(require-package 'org-fstree)
(require 'org-fstree)
;; (require-package 'org-mac-link)
;; (autoload 'org-mac-grab-link "org-mac-link" nil t)
;; (require-package 'org-mac-iCal)



;; ===== KEY BINDINGS =====
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

;; ===== PREFERENCES =====

(after-load 'org
  '(require 'ox-md nil t))

;; Show syntax highlighting per language native mode in *.org
(setq org-src-fontify-natively t)

;; For languages with significant whitespace like Python:
(setq org-src-preserve-indentation t)

;; (setq org-log-done t
;;       org-completion-use-ido t
;;       org-edit-timestamp-down-means-later t
;;       org-agenda-start-on-weekday nil
;;       org-agenda-span 14
;;       org-agenda-include-diary t
;;       org-agenda-window-setup 'current-window
;;       org-fast-tag-selection-single-key 'expert
;;       org-html-validation-link nil
;;       org-export-kill-product-buffer-when-displayed t
;;       org-tags-column 80)


;; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
;; (setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))

;; Targets start with the file name - allows creating level 1 tasks
;; (setq org-refile-use-outline-path (quote file))

;; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
;; (setq org-outline-path-complete-in-steps t)


;; ===== TODO =====
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
        (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)")))


;; ===== ORG CLOCK =====

;; Save the running clock and all clock history when exiting Emacs, load it on startup
;; (setq org-clock-persistence-insinuate t)
;; (setq org-clock-persist t)
;; (setq org-clock-in-resume t)

;; Change task state to STARTED when clocking in
;; (setq org-clock-in-switch-to-state "STARTED")

;; Save clock data and notes in the LOGBOOK drawer
;; (setq org-clock-into-drawer t)

;; Removes clocked tasks with 0:00 duration
;; (setq org-clock-out-remove-zero-time-clocks t)

;; Show clock sums as hours and minutes, not "n days" etc.
;; (setq org-time-clocksum-format
;;       '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;; Show the clocked-in task - if any - in the header line
;; (defun sanityinc/show-org-clock-in-header-line ()
;;   (setq-default header-line-format '((" " org-mode-line-string " "))))

;; (defun sanityinc/hide-org-clock-from-header-line ()
;;   (setq-default header-line-format nil))

;; (add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
;; (add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
;; (add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

;; (after-load 'org-clock
;;   (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
;;   (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))


;; ===== ORG POMODORO =====

;; (require-package 'org-pomodoro)
;; (after-load 'org-agenda
;;   (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))


;; ===== ICAL =====

;; ;; Show iCal calendars in the org agenda
;; (when (and *is-a-mac* (require 'org-mac-iCal nil t))
;;   (setq org-agenda-include-diary t
;;         org-agenda-custom-commands
;;         '(("I" "Import diary from iCal" agenda ""
;;            ((org-agenda-mode-hook #'org-mac-iCal)))))

;;   (add-hook 'org-agenda-cleanup-fancy-diary-hook
;;             (lambda ()
;;               (goto-char (point-min))
;;               (save-excursion
;;                 (while (re-search-forward "^[a-z]" nil t)
;;                   (goto-char (match-beginning 0))
;;                   (insert "0:00-24:00 ")))
;;               (while (re-search-forward "^ [a-z]" nil t)
;;                 (goto-char (match-beginning 0))
;;                 (save-excursion
;;                   (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
;;                 (insert (match-string 0))))))


(after-load 'org
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  (define-key org-mode-map (kbd "M-h") nil)
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link))


;; ===== GRAPHVIZ =====

(after-load 'org
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot)))
;;   (org-babel-do-load-languages 'org-babel-load-languages '((dot . t))))


;; ===== MOBILE ORG =====

(require 'org-mobile)
(setq org-directory "~/org")
(setq org-agenda-files (quote ("~/org/learning.org" "~/org/tasks.org")))
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-mobile-inbox-for-pull "~/org/flagged.org")


;; ===== LATEX =====

(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
(setq org-latex-custom-lang-environments
      '((LilyPond "lilypond")))

(setq org-latex-custom-lang-environments nil)


;; ===== PROJECTS =====

(setq org-publish-project-alist
      '(("notes"
         :base-directory "~/org/notes/"
         :base-extension "org"
         :exclude "\\(notes\\|README\\).org"
         :publishing-directory "~/org/notes/public_html/"
         :publishing-function org-html-publish-to-html)))

;; https://github.com/yurrriq/exercism/blob/master/org/projects.el
(load-file "~/src/yurrriq/exercism/org/projects.el")


;; ===== BABEL =====

(require-package 'babel)
(require 'babel)
(require 'ob-shell)

(after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (clojure . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . t)
     (latex . t)
     (python . t)
     (ruby . t)
     (sh . t))))

;; Use cider as the clojure execution backend
;; (setq org-babel-clojure-backend 'cider)


;; ===== EXPORT =====

(require-package 'ox-gfm)


;;; org-html-checkbox
;; from Sacha Chua's Emacs tweaks:
;; http://sachachua.com/blog/2014/03/emacs-tweaks-export-org-checkboxes-using-utf-8-symbols/

;; (defun sacha/org-html-checkbox (checkbox)
;;   "Format CHECKBOX into HTML."
;;   (case checkbox (on "<span class=\"check\">&#x2611;</span>") ; checkbox (checked)
;;         (off "<span class=\"checkbox\">&#x2610;</span>")
;;         (trans "<code>[-]</code>")
;;         (t "")))
;; (defadvice org-html-checkbox (around sacha activate)
;;   (setq ad-return-value (sacha/org-html-checkbox (ad-get-arg 0))))

;;; org-html-checkbox
;; from
;; http://kdr2.com/tech/emacs/1405-orgmode-checkbox-unicode.html

(defun unicode-for-org-html-checkbox (checkbox)
  "Format `CHECKBOX' into Unicode Characters."
  (cl-case checkbox
    (on    "&#x22A0;")
    (off   "&#x25FB;")
    (trans "&#x22A1;")
    (t     "")))

(defadvice org-html-checkbox (around unicode-checkbox activate)
  (setq ad-return-value (unicode-for-org-html-checkbox (ad-get-arg 0))))


(provide 'init-org)
;;; init-org.el ends here
