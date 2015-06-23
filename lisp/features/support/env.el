(require 'f)

(defvar org-doing-support-path
  (f-dirname load-file-name))

(defvar org-doing-features-path
  (f-parent org-doing-support-path))

(defvar org-doing-root-path
  (f-parent org-doing-features-path))

(add-to-list 'load-path org-doing-root-path)

(require 'org-doing)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
