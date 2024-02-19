;;; initfile --- Summary -*- mode: emacs-lisp; lexical-binding: t; -*-

;;; Commentary:

;; Works on Emacs 26.1, 27.1 and 28.1
;; Uses builtin package.el and use-package.
;;
;; To install, simply symlink or add the following to ~/.emacs:
;;
;; ;;; Load generic version
;; ;;(package-initialize)
;; (load "~/dotfiles/.emacs")

;;; Code:

;; set garbage collector threshold to 20MB during startup, considerably speeding up emacs
(setq gc-cons-threshold (* 20 1024 1024))
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 2 1024 1024))))

;; disable running regexes on filenames when loading .el files, speeding up emacs
(setq-local file-name-handler-alist nil)

;; setup a cozy welcome screen
(setq inhibit-startup-message t)        ; No startup message in minibuffer
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message
      (format "%s\nconfig: %s"
              (replace-regexp-in-string "\n" "" (emacs-version))
              (abbreviate-file-name (or load-file-name buffer-file-name))))
(when (> (display-color-cells) 16)
  (if (>= emacs-major-version 28)
      (progn
        (require-theme 'modus-themes)

        ;; tone down the contrast between fg and bg
        (setq modus-themes-vivendi-color-overrides
              '((bg-main . "#050505") (fg-main . "#f0f0f0") (fg-max . "#ffffff"))
              modus-themes-operandi-color-overrides
              '((bg-main . "#fafafa") (fg-main . "#0f0f0f") (fg-max . "#000000")))

        (setq modus-themes-tabs-accented t
              modus-themes-fringes 'subtle
              modus-themes-mode-line '(borderless accented)
              modus-themes-hl-line '()
              modus-themes-prompts '(intense)
              modus-themes-bold-constructs nil
              modus-themes-italic-constructs t
              modus-themes-syntax '(yellow-comments green-strings)
              modus-themes-paren-match '(bold intense)
              modus-themes-region '(bg-only)
              modus-themes-completions '(opinionated)
              modus-themes-box-buttons '(0.9)
              ;; org-mode
              modus-themes-headings '((t . (rainbow)))
              modus-themes-scale-headings t
              modus-themes-org-blocks 'gray-background)

        (defun my/modus-themes-custom-faces ()
          ;; Override and define faces
          (modus-themes-with-colors
            (custom-set-faces
             `(font-lock-function-call-face ((,class :foreground ,magenta-faint)))
             `(font-lock-variable-use-face ((,class :foreground ,fg-main)))
             `(font-lock-property-use-face ((,class :foreground ,cyan-alt-faint)))
             `(font-lock-bracket-face ((,class :foreground ,(modus-themes-color 'fg-max))))
             `(font-lock-number-face ((,class :foreground ,yellow-faint)))
             )))
        (add-hook 'modus-themes-after-load-theme-hook #'my/modus-themes-custom-faces)

        (modus-themes-load-vivendi))
    (progn
      (load-theme 'wombat t)                          ; cool dark builtin theme
      (set-face-background 'default "#090909")        ; really dark for better contrast
      (set-face-foreground 'font-lock-comment-face "orangered") ; redish comments
      (set-face-background 'mode-line "firebrick")))) ; make active buffer more visible

;; setup package repos for M-x package-*
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities '(("gnu" . 5) ; default gnu elpa
                                   ("melpa" . 0)))
(when (< emacs-major-version 27)
  (package-initialize))
;;(package-install-selected-packages)

;;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)            ; log package loading times

(eval-when-compile
  (require 'use-package))
(unless (package-installed-p 'diminish)
  (package-install 'diminish))

(setq use-package-compute-statistics t) ; view the statistical report using `use-package-report'

;;; fix exec-path when windowed
(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell :ensure t
    :config
    (setq exec-path-from-shell-arguments (delete "-i" exec-path-from-shell-arguments))
    (exec-path-from-shell-initialize)))

;;; usual tweaks
(setq ring-bell-function 'ignore)                    ; NO stupid bell sound
(if (functionp 'scroll-bar-mode)(scroll-bar-mode -1)); No scrollbar
(if (functionp 'tool-bar-mode)(tool-bar-mode -1))    ; No toolbar
(unless (and (display-graphic-p) (eq system-type 'darwin)); No menubar except for graphical on
  (menu-bar-mode -1))                                     ; mac where it doesn't waste space
(fset 'yes-or-no-p 'y-or-n-p)                        ; 'y/n' instead of 'yes/no'
(setq line-number-mode t)                            ; Line number in modeline
(setq column-number-mode t)                          ; Column number in modeline
(unless (display-graphic-p) (size-indication-mode t)); Show size of file in modeline
(setq font-lock-maximum-decoration t)                ; Unleash a rainbow of colors
(setq diff-font-lock-prettify t)                     ; Prettier diff-mode
(global-font-lock-mode t)                            ; Enable colors
(show-paren-mode t)                                  ; Highlight matching paren
(auto-image-file-mode t)                             ; Open images NOT in raw data
(setq scroll-step 3)                                 ; Scroll lines 3 by 3
(setq next-screen-context-lines 3)                   ; Page up/down keep 3 lines
(setq mouse-yank-at-point t)                         ; Mouse paste on text pointer
(setq scroll-preserve-screen-position t)             ; Page up/down preserve point on screen
(setq kill-whole-line t)                             ; Cut whole line when point is at col0
(delete-selection-mode t)                            ; Overwrite region selected
(setq uniquify-buffer-name-style 'forward)           ; Add parent dirs to buffer names when ambiguous
(set-default 'indicate-empty-lines t)                ; Mark end of file lines in fringe
(set-default 'indicate-buffer-boundaries 'left)      ; Mark buffer boundaries in fringe
(setq make-backup-files nil)                         ; Do not create backup~ files
(setq delete-old-versions t)                         ; Silently delete old backup~ files
(setq delete-auto-save-files t)                      ; Delete #autosave# files on save
;;(global-auto-revert-mode t)                        ; Auto reload files when modified
;;(dynamic-completion-mode t)
(setq compilation-scroll-output 'first-error)        ; Compile output auto-scroll up to an error
(setq-default enable-remote-dir-locals t)            ; Tramp handles dir-local files
(setq-default tab-width 4)                           ; Tabs are 4 chars wide
(setq-default indent-tabs-mode nil)                  ; Use spaces for indentation
(setq x-stretch-cursor t)                            ; Draw cursor as wide as the glyph under point
(setq auto-window-vscroll nil)                       ; Workaround performance issues with next-line
(when (>= emacs-major-version 27) (global-so-long-mode 1)); Mitigate slowness due to very long lines

;; forbid moving point into read-only part of the prompt
(plist-put minibuffer-prompt-properties 'point-entered 'minibuffer-avoid-prompt)

;; clickable URLs
(add-hook 'text-mode-hook 'goto-address-mode)
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(add-hook 'conf-mode-hook 'goto-address-prog-mode)

;; show line numbers
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)

;; show trailing whitespaces (source code only)
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; delete trailing whitespaces on save (source code only)
(add-hook 'before-save-hook (lambda() (when (derived-mode-p 'prog-mode)
                                        (delete-trailing-whitespace))))

;; frame title
(setq frame-title-format '("%b" (buffer-file-name ": %f") " [" (:eval mode-name) "]"))

;; doom-modeline -- Neat modeline
(when (display-graphic-p)
  (use-package all-the-icons :ensure t)  ; make sure to run `M-x all-the-icons-install-fonts` once
  (use-package doom-modeline :ensure t
    :init (doom-modeline-mode 1)
    :custom
    (doom-modeline-buffer-encoding :nondefault)
    (doom-modeline-checker-simple-format nil) ; detailed numbers of info/warn/err
    (doom-modeline-vcs-max-length 18)
    (doom-modeline-buffer-file-name-style 'truncate-upto-project)
    (doom-modeline-hud t)                 ; show buffer position/size as a scrollbar/minimap in the left-most part of the modeline
    (doom-modeline-bar-width 8)           ; make the bar same size as the fringe
    (doom-modeline-percent-position nil)  ; redundant with the hud
    (doom-modeline-irc nil)))

;; pixel-scroll-precision -- Smooth scrolling
(use-package pixel-scroll :ensure nil
  :if (and (>= emacs-major-version 29) (display-graphic-p))
  :init (pixel-scroll-precision-mode 1)
  ;; unbind page up/down - otherwise they do not work in ivy
  :bind (:map pixel-scroll-precision-mode-map (("<prior>" . nil)
                                               ("<next>" . nil))))

;;; hl-line -- Highlight current line everywhere
(use-package hl-line :ensure nil
  :if (> (display-color-cells) 16)
  :init (global-hl-line-mode))

;;; windmove -- [M-arrows]/[M-S-arrows] to move/swap buffer in direction
(use-package windmove :ensure nil
  :bind (("M-<up>" . windmove-up)
         ("M-<down>" . windmove-down)
         ("M-<left>" . windmove-left)
         ("M-<right>" . windmove-right)))
(use-package windmove :ensure nil
  :if (>= emacs-major-version 27)
  :bind (("M-S-<up>" . windmove-swap-states-up)
         ("M-S-<down>" . windmove-swap-states-down)
         ("M-S-<left>" . windmove-swap-states-left)
         ("M-S-<right>" . windmove-swap-states-right)))

;; fallback to buffer-move when windmove-swap-states is not available (emacs 26)
(use-package buffer-move :ensure t
  :if (< emacs-major-version 27)
  :bind (("M-S-<up>" . buf-move-up)
         ("M-S-<down>" . buf-move-down)
         ("M-S-<left>" . buf-move-left)
         ("M-S-<right>" . buf-move-right)))

;;; ivy -- interactive completion for most things
(use-package ivy :ensure t
  :diminish ivy-mode
  :commands ivy-completing-read
  :init ; this was extracted from ivy-mode function to allow lazy-loading
  (setq completing-read-function 'ivy-completing-read)
  :config
  (use-package flx :ensure t)        ; automatically used for ivy--regex-fuzzy
  (ivy-mode 1)                       ; properly enable ivy everywhere
  (setq ivy-use-virtual-buffers t)   ; add recent buffers when switching buffers
  (setq ivy-count-format "(%d/%d) ") ; display number of matches in prompt
  (setq ivy-initial-inputs-alist nil)
  ;; somewhat hide rarely used buffers
  (add-to-list 'ivy-ignore-buffers "^\*")
  (add-to-list 'ivy-ignore-buffers "^[tT][aA][gG][sS]$")
  :bind (([remap switch-to-buffer] . ivy-switch-buffer)
         ([remap switch-to-buffer-other-window] . ivy-switch-buffer-other-window)))

;; display additional information for completion items (ie: switch-buffer shows major-modes)
(use-package ivy-rich :ensure t :defer t
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full)
  (ivy-rich-switch-buffer-align-virtual-buffer t)
  (ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode 1))

;; even better completion for some functions powered by ivy
(use-package counsel :ensure t
  :config
  (use-package smex :ensure t)       ; automatically used for counsel-M-x
  (ivy-rich-reload)                  ; ensure ivy-rich also applies to counsel functions
  ;; use fuzzy flx matching by default for M-x
  (add-to-list 'ivy-re-builders-alist '(counsel-M-x . ivy--regex-fuzzy))
  (add-to-list 'ivy-re-builders-alist '(counsel-git . ivy--regex-fuzzy))
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h S" . counsel-info-lookup-symbol)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c J" . counsel-rg)
         ("C-c i" . counsel-imenu)
         ("M-y" . counsel-yank-pop)))

;; incremental search powered by ivy
(use-package swiper :ensure t
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

;;; tooltips for currently entered command (prefix)
(use-package which-key :ensure t
  :diminish ""
  :config (which-key-mode)
  :custom
  (which-key-add-column-padding 1)
  (which-key-idle-secondary-delay 0))

;;; expand region semantically around cursor
(use-package expand-region :ensure t
  :bind ("C-=" . er/expand-region))

;;; projectile
(use-package projectile :ensure t :defer t
  :config
  ;; delay loaded until we visit a non virtual buffer
  ;; we manually call projectile's find-file hook otherwise the first real file visited would have incorrect modeline
  ;; TODO more correct way to do this?
  (defun my-projectile-find-file-hook-first-time ()
    (projectile-mode +1)
    (projectile-find-file-hook-function)
    (remove-hook 'find-file-hook 'my-projectile-find-file-hook-first-time)
    (remove-hook 'dired-mode-hook 'my-projectile-find-file-hook-first-time))
  :custom
  (projectile-completion-system 'ivy)
  (projectile-mode-line-prefix " P")
  :hook ((find-file . my-projectile-find-file-hook-first-time)
         (dired-mode . my-projectile-find-file-hook-first-time))
  :bind (:map projectile-mode-map ("C-c p" . projectile-command-map)))

;;; magit -- Git interface
(use-package magit :ensure t :defer t
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq exec-path (add-to-list 'exec-path "C:/Program Files (x86)/Git/bin"))
  :bind ("C-x g" . magit-status))

;;; treemacs -- sidebar tree layout file explorer
(use-package treemacs :defer t
  :config
  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;(treemacs-resize-icons 44)
  (treemacs-fringe-indicator-mode 'only-when-focused)  ; BUG? does not actually apply
  (treemacs-git-commit-diff-mode t)
  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))

  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t) (treemacs-git-mode 'deferred))
    (`(t . _) (treemacs-git-mode 'simple))))
(use-package treemacs-projectile :after (treemacs projectile))
(use-package treemacs-magit :after (treemacs magit))

;;; .editorconfig support
(use-package editorconfig :ensure t :defer t
  :diminish ""
  :init
  ;; delay loaded until we visit a non virtual buffer
  (add-hook 'prog-mode-hook (lambda () (when buffer-file-name (editorconfig-mode 1))))
  (add-hook 'text-mode-hook (lambda () (when buffer-file-name (editorconfig-mode 1)))))

;;; Browse on web UI
(use-package browse-at-remote :ensure t :defer t
  :custom (browse-at-remote-add-line-number-if-no-region-selected nil)
  :bind (("M-g r" . browse-at-remote)
         ("M-g R" . browse-at-remote-kill)))

;;; Make the file executable using sh-set-shell or executable-set-magic
(use-package executable :ensure nil :defer t
  :custom (executable-prefix-env t))

;;; Jenkinsfile files support
(use-package groovy-mode :ensure t :defer t)

;;; Dockerfile files support
(use-package dockerfile-mode :ensure t :defer t)

;;; .git{ignore,config,attributes} files support
(use-package git-modes :ensure t :defer t)

;;; CMakeLists.txt files support
(use-package cmake-mode :defer t)

;;; Markdown files support
(use-package markdown-mode :ensure t :defer t
  :custom
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-fontify-code-blocks-natively t))

;;; Flycheck everything
(use-package flycheck :ensure t
  :init
  (global-flycheck-mode))

;;; show ansi colors in compile buffer
(use-package ansi-color :ensure nil
  :config
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . my-colorize-compilation-buffer))

;;; Company completion menu for everything
(use-package company :ensure t
  :diminish ""
  :init
  (global-company-mode t)
  :config
  (when (display-graphic-p) (company-quickhelp-mode 1))
  :bind ("M-p" . company-complete-common)
  :custom
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.2))
(when (display-graphic-p)
  (use-package company-quickhelp :ensure t :defer t))

;;; LSP
(use-package lsp-mode :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui :ensure t
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-ivy :ensure t
  :commands lsp-ivy-workspace-symbol)

;;; C/C++
(use-package cc-mode :ensure nil
  :config
  (defvaralias 'c-basic-offset 'tab-width)
  (add-to-list 'c-default-style '(c-mode . "bsd")) ; better default style
  (add-to-list 'c-default-style '(c++-mode . "bsd")) ; better default style
  (c-set-offset 'innamespace [0])    ; do not indent namespace content
  (c-set-offset 'inextern-lang [0])  ; do not indent extern "C" content
  ;; (use-package smart-tabs-mode :ensure t) ; better tab indentation using ...
  ;; (smart-tabs-insinuate 'c++)             ; ... spaces for alignement
  (use-package modern-cpp-font-lock :ensure t
    :diminish modern-c++-font-lock-mode
    :hook (c++-mode-hook . modern-c++-font-lock-mode))
  (use-package clang-format :ensure t :defer t)
  :bind (:map c-mode-map
              ("C-c o" . ff-find-other-file)
         :map c++-mode-map
              ("C-c o" . ff-find-other-file)
              ("C-c f" . clang-format)
              ("C-c F" . clang-format-buffer)))

;;; Python IDE
(use-package elpy :ensure t :defer t
  :commands elpy-enable
  :init (advice-add 'python-mode :before 'elpy-enable)
  :config
  ;; use flycheck rather than flymake
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  :custom
  (elpy-rpc-python-command "python3")
  (elpy-rpc-ignored-buffer-size 409600) ; default is 102400
  (elpy-rpc-timeout 2)                  ; default is 1
  :bind (:map elpy-mode-map
              ("C-c f" . elpy-format-code)
              ;; restore windmove keybinds
              ("M-<up>" . nil)
              ("M-<down>" . nil)
              ("M-<left>" . nil)
              ("M-<right>" . nil)))

;;; Typescript IDE
;; for yarn PnP support, run:
;;   YARN_ENABLE_IMMUTABLE_CACHE=false YARN_ENABLE_NETWORK=true yarn dlx @yarnpkg/sdks base
;; also see: https://github.com/ramblehead/.emacs.d/blob/master/lisp/yarn-pnp.el
(use-package typescript-mode :defer t)
(use-package tide :defer t
  :after (:all (:any typescript-mode js) company flycheck)
  :init
  ;; somehow js-mode does not work with :hook, so here we go:
  (add-hook 'js-mode-hook #'tide-setup)
  (add-hook 'js-mode-hook #'tide-hl-identifier-mode)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         ))
(use-package web-mode
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (defun my/setup-tide-for-tsx()
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (tide-setup)
      (tide-hl-identifier-mode)))
  :hook (web-mode . my/setup-tide-for-tsx)
  :mode ("\\.tsx$" . web-mode))

;;; Go IDE
(use-package go-mode :defer t
  :config
  ;; format file before save using goimports
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (use-package company-go :ensure t
    :bind (:map go-mode-map
                ("M-." . godef-jump)))
  (push 'company-go company-backends)
  (use-package go-eldoc :ensure t)
  (add-hook 'go-mode-hook 'go-eldoc-setup))

;;; .tf files support
(use-package terraform-mode :defer t
  :config
  (company-terraform-init))
(use-package company-terraform :defer t)

;;; .json files support
(use-package json-mode :ensure t :defer t)

;;; .proto files support
(use-package protobuf-mode :ensure t :defer t)

;;; .thrift files support
(use-package thrift :defer t
  :config
  (setq thrift-indent-level 4)
  :mode ("\\.thrift$" . thrift-mode))

;;; .feature files support
(use-package feature-mode :ensure t :defer t)

;;; .scala files support
(use-package scala-mode :ensure t :defer t
  :interpreter
  ("scala" . scala-mode))

;;; .lua files support
(use-package lua-mode :defer t)

;;; .yml files support
(use-package yaml-mode :ensure t
  :mode ("\\.yml$" . yaml-mode))

;;; 3D .obj/.mtl files support
(use-package wavefront-obj-mode
  :mode (("\\.obj$" . wavefront-obj-mode)
         ("\\.mtl$" . wavefront-obj-mode)))

;;; tweaks when console
(unless (display-graphic-p)
  (xterm-mouse-mode)                                 ; enable xterm mouse support
  (global-set-key [mouse-4] 'scroll-down-line)       ; mouse wheel scrolls
  (global-set-key [mouse-5] 'scroll-up-line)         ; mouse wheel scrolls
  (normal-erase-is-backspace-mode -1)                ; Fix delete key
  ;; fix keymaps
  (define-key function-key-map "\e[1;5H" [C-home])
  (define-key function-key-map "\e[1;5F" [C-end])
  (define-key function-key-map "\e[1;5A" [C-up])
  (define-key function-key-map "\e[1;5B" [C-down])
  (define-key function-key-map "\e[1;5D" [C-left])
  (define-key function-key-map "\e[1;5C" [C-right])
  (define-key function-key-map "\e[1;3D" [M-left])
  (define-key function-key-map "\e[1;3C" [M-right])
  (define-key function-key-map "\e[1;3A" [M-up])
  (define-key function-key-map "\e[1;3B" [M-down])
  (define-key function-key-map "\e[1;10D" [S-M-left])
  (define-key function-key-map "\e[1;10C" [S-M-right])
  (define-key function-key-map "\e[1;10A" [S-M-up])
  (define-key function-key-map "\e[1;10B" [S-M-down]))

;;; tweaks when graphical
(when (display-graphic-p)
  (blink-cursor-mode 0)                                ; static cursor
  (add-to-list 'initial-frame-alist '(height . 48))    ; Slightly bigger initial frame
  (add-to-list 'initial-frame-alist '(width . 120))    ; Slightly bigger initial frame
  (global-unset-key "\C-z"))                           ; only keep C-x z to suspend-frame

;;; custom keyboard binds
(global-set-key [mouse-3] 'imenu)                        ; Right-click list of functions
(global-set-key [C-M-up] 'scroll-down-line)              ; Scroll line by line...
(global-set-key [C-M-down] 'scroll-up-line)              ; ...leaving point in place
(global-set-key (kbd "M-SPC") 'cycle-spacing)            ; a more versatile just-one-space
(global-set-key (kbd "M-u") 'upcase-dwim)                ; region-aware upcase-word
(global-set-key (kbd "M-l") 'downcase-dwim)              ; region-aware downcase-word
(global-set-key (kbd "M-c") 'capitalize-dwim)            ; region-aware capitalize-word

;; [C-$] Toggle hiding indented lines based on point position
(defun selective-display-on-column-at-point ()
  "Activate selective display based on the column at point."
  (interactive)
  (set-selective-display (if selective-display nil (+ 1 (current-column)))))
(global-set-key [(control $)] 'selective-display-on-column-at-point)

;;; Replace GNU advertising
(defun display-startup-echo-area-message ()
  "Display time since startup."
  (message "All done in %s with %s gcs, %s. \\o/" (emacs-init-time) gcs-done (user-login-name)))

(provide '.emacs)
;;; .emacs ends here
