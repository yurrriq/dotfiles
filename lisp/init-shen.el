(add-hook 'shen-mode-hook 'auto-complete-mode)
(add-hook 'shen-mode-hook 'hs-minor-mode)
(add-hook 'shen-mode-hook 'paredit-mode)
(add-hook 'shen-mode-hook
	  (lambda ()
	    (setq prettify-symbols-alist
		  '(("/." . 955)	; λ
		    ("<-" . 8592)       ; ←
		    ("->" . 8594)	; →
		    ("-->" . 10230)     ; ⟶
		    ("=>" . 8658)	; ⇒
		    ("map" . 8614))) 	; ↦
	    (prettify-symbols-mode)))


(provide 'init-shen)
