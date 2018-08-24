;;; test-helper --- Test helper for org-doing

;;; Commentary:
;; test helper inspired from https://github.com/tonini/overseer.el/blob/master/test/test-helper.el

;;; Code:

(require 'f)

(defvar cpt-path
  (f-parent (f-this-file)))

(defvar org-doing-test-path
  (f-dirname (f-this-file)))

(defvar org-doing-root-path
  (f-parent org-doing-test-path))

(defvar org-doing-sandbox-path
  (f-expand "sandbox" org-doing-test-path))

(when (f-exists? org-doing-sandbox-path)
  (error "Something is already in %s. Check and destroy it yourself" org-doing-sandbox-path))

(defmacro within-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory org-doing-sandbox-path))
     (when (f-exists? org-doing-sandbox-path)
       (f-delete default-directory :force))
     (f-mkdir org-doing-sandbox-path)
     ,@body
     (f-delete default-directory :force)))

(require 'ert)
(require 'org-doing (f-expand "org-doing" org-doing-root-path))

(provide 'test-helper)
;;; test-helper.el ends here
