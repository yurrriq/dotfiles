(require-package 'clojure-mode)
;; (require-package 'cljsbuild-mode)
(require-package 'elein)


;; ==== INDENDATION =====

(defun yurrriq/customize-clojure-indent ()
  (define-clojure-indent
    (ANY 2)
    (DELETE 2)
    (GET 2)
    (HEAD 2)
    (OPTIONS 2)
    (POST 2)
    (PUT 2)
    (alter-var-root 1)
    (assoc 1)
    (assoc-in 1)
    (apply 1)
    (condp-> 'defun)
    (context 2)
    (contract 1)
    (defroutes 'defun)
    (branch 'defun)
    (mlet 'defun)
    (dissoc 1)
    (ffirst 0)
    (go-let 1)
    (lazy-seq 0)
    (list 0)
    (match 1)
    (merge 0)
    (not-join 'defun)
    (nwhen-let 1)
    (partial 'defun)
    (reset! 0)
    (second 0)
    (some-> 1)
    (some->> 1)
    (thunk-timeout 0)
    (update-in 1)
    ;; (-> 1)
    ;; (->> 1)
    ;; (cond-> 1)
    ;; (start 1)
    ;; (system-map 0)
    ;;
    ;;
    ;; Expectations -- http://jayfields.com/expectations/emacs-tweaks.html
    (expect 'defun)
    (expect-let 'defun)
    (given 'defun)
    (context 1)
    (freeze-time 1)
    (redef-state 1)
    (from-each 1)))

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(clojure-defun-indents nil)
;;  '(clojure-defun-style-default-indent 2))

;; ===== FONT LOCK =====

(defun yurrriq/customize-clojure-font-lock ()
  (font-lock-add-keywords
   'clojure-mode
   '(("\\(#\\)[^_]("
      (0 (progn (compose-region (match-beginning 1)
				(match-end 1) "ƒ")
		nil)))

     ("\\(#\\){"
      (0 (progn (compose-region (match-beginning 1)
				(match-end 1) "∈")
		nil)))
     ("(\\(fn\\)[[:space:]]"
      (0 (progn (compose-region (match-beginning 1)
				(match-end 1) "λ")
		nil)))
     ("(\\(not=\\)[[:space:]]"
      (0 (progn (compose-region (match-beginning 1)
				(match-end 1) "≠")
		nil)))
     ("([[:space:]]\\(-->\\)[[:space:]]"
      (0 (progn (compose-region (match-beginning 1)
				(match-end 1) "≠")
		nil)))
     ;; ("(\\(partial\\)[[:space:]]"
     ;;  (0 (progn (compose-region (match-beginning 1)
     ;;                            (match-end 1) "Ƥ")
     ;;            nil)))
     ;; ("(\\(map\\)[[:space:]]"
     ;;  (0 (progn (compose-region (match-beginning 1)
     ;;                            (match-end 1) "↦")
     ;;            nil)))
     ;; ("(\\(and\\)[[:space:]]"
     ;; 	(0 (progn (compose-region (match-beginning 1)
     ;; 				  (match-end 1) "∧")
     ;; 		  nil)))
     ;; ("(\\(or\\)[[:space:]]"
     ;; 	(0 (progn (compose-region (match-beginning 1)
     ;; 				  (match-end 1) "∨")
     ;; 		  nil)))
     ;; ("(\\(comp\\)[[:space:]]"
     ;;  (0 (progn (compose-region (match-beginning 1)
     ;;                            (match-end 1) "∘")
     ;;            nil)))
     )))


;; ===== OUTSHINE =====

;; (require 'outshine)
;; (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
;; (add-hook 'clojure-mode-hook 'outline-minor-mode)


;; ===== CUSTOMIZATIONS AND HOOKS =====

(after-load 'clojure-mode
  (yurrriq/customize-clojure-font-lock)
  (yurrriq/customize-clojure-indent)
  (add-hook 'clojure-mode-hook 'eldoc-mode)
  ;; org-mode html export hates this...
  ;; (add-hook 'clojure-mode-hook 'fci-mode)
  (add-hook 'clojure-mode-hook 'hs-minor-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook 'sanityinc/lisp-setup))


(provide 'init-clojure)
