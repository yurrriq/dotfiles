;;; init-dash-at-point.el --- Open Dash docset based on the the point and mode.

;;; Commentary:
;; Dash -- https://kapeli.com/dash

;;; Code:

(require-package 'dash-at-point)
(require 'dash-at-point)

(autoload 'dash-at-point "dash-at-point"
  "Search the word at point with Dash."
  t)

(global-set-key (kbd "C-c d") 'dash-at-point)

(global-set-key (kbd "C-c e") 'dash-at-point-with-docset)

(provide 'init-dash-at-point)
;;; init-dash-at-point.el ends here
