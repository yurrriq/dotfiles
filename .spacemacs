;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  ;; HACK
  ;; (setq with-editor-emacsclient-executable "/run/current-system/sw/bin/emacsclient")
  (setq with-editor-emacsclient-executable "/usr/local/bin/emacsclient")
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs-base
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/src/yurrriq/spacemacs-private/")
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   `(
     ruby
     ruby
     javascript
     python
     csv
     ;; ruby
     ;; sml
     ;; ocaml
     ;; FIXME: rust
     ;; python
     html
     ;; +checkers
     (spell-checking :variables
                     spell-checking-enable-by-default nil)
     syntax-checking

     ;; +completion
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t)
     (helm :packages
           helm
           helm-ag
           (helm-spacemacs-help :location local)
           imenu
           popwin
           ;; projectile
           )

     ;; +emacs
     (better-defaults :variables
                      better-defaults-move-to-beginning-of-code-first t)
     ;; (org :variables org-enable-github-support t)
     ;; org
     smex
     ;; (typography :variables typography-enable-typographic-editing t)

     ;; +fun
     emoji

     ;; +lang
     ;; agda
     ;; c
     (clojure :variables clojure-enable-fancify-symbols t)
     common-lisp
     ;; coq
     ;; elixir
     elm
     emacs-lisp
     erlang
     fsharp
     (go :variables go-tab-width 4)
     graphviz
     (haskell :variables haskell-enable-hindent-style "chris-done")
     ;; html
     idris
     ;; javascript
     latex
     ;; lua
     (markdown :variables markdown-live-preview-engine 'vmd)
     ;; nix
     ;; ocaml
     ;; php
     ;; purescript
     rust
     ;; scheme
     shell-scripts
     sml
     ;; sql
     swift
     yaml

     ;; +os
     ;; osx

     ;; +source-control
     git
     github

     ;; +spacemacs
     (spacemacs-editing-visual :packages rainbow-delimeters)
     (spacemacs-editing :packages origami spacemacs-whitespace-cleanup)
     ;; spacemacs-org
     (spacemacs-ui-visual :packages
                          fill-column-indicator
                          hl-todo
                          golden-ratio)
     (spacemacs-ui :packages paradox)

     (colors :variables
             colors-enable-nyan-cat-progress-bar ,(display-graphic-p))
     (shell :variables
            shell-default-term-shell "/run/current-system/sw/bin/fish"
            shell-default-height 30
            shell-default-position 'bottom)

     ;; +tools
     dash

     ;; Custom
     lfe
     lilypond
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(paredit
                                      protobuf-mode
                                      ;; flycheck-protobuf
                                      http
                                      nginx-mode
                                      multiple-cursors
                                      wakatime-mode
                                      shen-mode
                                      rainbow-delimiters
                                      nix-mode
                                      org-plus-contrib
                                      ox-gfm
                                      htmlize
                                      ess
                                      dockerfile-mode
                                      nix-buffer)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(;; evil-escape
                                    ;; evil-magit
                                    ;; evil-surround
                                    ;; evil-visualstar
                                    ;; exec-path-from-shell
                                    undo-tree)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq custom-file "~/.emacs.d/private/local/custom.el")
  ;; HACK for recent magit
  (setq exec-path-from-shell-arguments nil)
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'emacs
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                ;; (projects . 7)
                                )
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(; spacemacs-dark spacemacs-light
                         monokai leuven)
   dotspacemacs-colorize-cursor-according-to-state nil
   dotspacemacs-default-font '("Hack"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key nil ;; default ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native t
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'origami
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server t
   dotspacemacs-search-tools '("ag" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing

   ;; +lang/javascript
   js2-basic-offset 2
   js-indent-level 2))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (add-to-list 'load-path "/run/current-system/sw/share/emacs/site-lisp")
  (add-to-load-path "~/lib/emacs/apib-mode")
  (autoload 'apib-mode "apib-mode"
    "Major mode for editing API Blueprint files" t)
  (add-to-list 'auto-mode-alist '("\\.apib\\'" . apib-mode)))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  (global-set-key (kbd "M-l") (lambda () (interactive) (insert "λ")))
  (global-set-key (kbd "M-f") (lambda () (interactive) (insert "ƒ")))
  (global-set-key (kbd "M--") (lambda () (interactive) (insert "→")))
  (global-set-key (kbd "M-s-ƒ") 'helm-do-grep-ag)

  (global-set-key (kbd "C-c d") 'dash-at-point)
  (global-set-key (kbd "C-c e") 'dash-at-point-with-docset)

  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

  ;; (global-git-commit-mode t)

  (global-set-key (kbd "s-r") 'spacemacs/helm-jump-in-buffer)


  (require 'mmm-mode)

  (require 'mmm-auto)

  (mmm-add-classes
   '((markdown-erlang
      :submode erlang-mode
      :face mmm-default-submode-face
      :front "^```erlang[\n\r]+"
      :back "^```$")
     (markdown-lfe
      :submode lfe-mode
      :face mmm-default-submode-face
      :front "^```\\(?:{\\.lfe.*}\\|lfe\\)[\n\r]+"
      :back "^```$")
     (markdown-fish
      :submode fish-mode
      :face mmm-default-submode-face
      :front "^```fish[\n\r]+"
      :back "^```$")
     (markdown-idris
      :submode idris-mode
      :face mmm-default-submode-face
      :front "^```\\(?:{\\.idris.*}\\|idris\\)[\n\r]+"
      :back "^```$")
     (scheme-lilypond
      :submode LilyPond-mode
      :face    mmm-default-submode-face
      :front   "^\s+#{"
      :back    "^\s+#}")))

  (setq mmm-global-mode 'maybe)
  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-erlang)
  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-fish)
  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-lfe)
  (mmm-add-mode-ext-class 'scheme-mode nil 'scheme-lilypond)

  ;; Literate Idris
  (mmm-add-mode-ext-class 'idris-mode ".lidr" 'markdown-idris)

  ;; (setq mmm-submode-decoration-level 1)

  ;; FIXME
  ;; (load-file (let ((coding-system-for-read 'utf-8))
  ;;              (shell-command-to-string "agda-mode locate")))
  (yas-global-mode 1)

  ;; FIXME
  ;; (org-babel-do-load-languages
  ;;  'org-babel-load-languages
  ;;  '((emacs-lisp . t)
  ;;    (haskell . t)
  ;;    (latex . t)
  ;;    (sh . t)
  ;;    (coq . t)
  ;;    (java . t)
  ;;    (R . t)
  ;;    ))

  ;; (global-undo-tree-mode 0)

  (require 'wakatime-mode)
  (setq wakatime-cli-path "/run/current-system/sw/lib/python2.7/site-packages/wakatime/cli.py")
  (setq wakatime-python-bin "/run/current-system/sw/bin/python2")
  (global-wakatime-mode)

  (require 'clojure-mode)

  (define-clojure-indent
    (defroutes 'defun)
    (OPTIONS 2)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)

    (some-> 'defun)
    (cond-> 'defun)
    (cond->> 'defun)
    (some->> 'defun)
    (some-<> 'defun)
    ;; plink
    (db/insert 'defun)
    (http/get 'defun)
    (j/jquery 'defun)
    (json-response 'defun)
    (response 'defun)
    (html 'defun)
    (html5 'defun)
    (handlers/burned-plink 2)
    ;; (burn-plink :defn)
    )

  (dolist (m '(clojure-mode-hook cider-repl-mode-hook))
    (add-hook m #'paredit-mode)
    (add-hook m #'rainbow-delimiters-mode))

  ;; From ess-noweb-mode.el
  ;; Put this into your ~/.emacs to use this mode automagically.
  (autoload 'ess-noweb-mode "ess-noweb-mode" "Editing noweb files." t)
  (setq auto-mode-alist (append (list (cons "\\.nw$" 'ess-noweb-mode))
                                auto-mode-alist))

  (spacemacs/toggle-global-whitespace-cleanup-on)

  ;; wtf?!
  (global-undo-tree-mode 0)

  ;; FIXME
  ;; (eval-after-load 'flycheck '(flycheck-clojure-setup))
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)

  ;; (require 'flycheck-tip)
  ;; (flycheck-tip-use-timer 'verbose)

  (define-key origami-mode-map (kbd "C-c f") 'origami-recursively-toggle-node)
  (define-key origami-mode-map (kbd "C-c F") 'origami-toggle-all-nodes)
  )
