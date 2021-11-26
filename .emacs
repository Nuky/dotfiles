;;; initfile --- Summary -*- mode: emacs-lisp; lexical-binding: t; -*-

;;; Commentary:

;; Works on Emacs 26.1 and 27.1
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
  (load-theme 'wombat t)                        ; cool dark builtin theme
  (set-face-underline 'highlight nil)           ; fix wombat theme with hl-line
  (set-face-foreground 'highlight nil)          ; fix wombat theme with hl-line
  (set-face-background 'default "#090909")      ; really dark for better contrast
  (set-face-background 'highlight "#1A1A1A")    ; darker highlight, more subtle
  (set-face-foreground 'font-lock-comment-face "orangered") ; redish comments
  (set-face-background 'mode-line "firebrick")) ; make active buffer more visible

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
(use-package exec-path-from-shell :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-arguments (delete "-i" exec-path-from-shell-arguments))
  (exec-path-from-shell-initialize))

;;; usual tweaks
(setq ring-bell-function 'ignore)                    ; NO stupid bell sound
(if (functionp 'scroll-bar-mode)(scroll-bar-mode -1)); No scrollbar
(if (functionp 'tool-bar-mode)(tool-bar-mode -1))    ; No toolbar
(unless (and (display-graphic-p) (eq system-type 'darwin)); No menubar except for graphical on
  (menu-bar-mode -1))                                     ; mac where it doesn't waste space
(fset 'yes-or-no-p 'y-or-n-p)                        ; 'y/n' instead of 'yes/no'
(setq line-number-mode t)                            ; Line number in modeline
(setq column-number-mode t)                          ; Column number in modeline
(display-time)                                       ; Time in modeline
(setq display-time-24hr-format t)                    ; 24h time format in modeline
(size-indication-mode t)                             ; Show size of file in modeline
(setq font-lock-maximum-decoration t)                ; Unleash a rainbow of colors
(setq diff-font-lock-prettify t)                     ; Prettier diff-mode
(global-font-lock-mode t)                            ; Enable colors
(show-paren-mode t)                                  ; Highlight matching paren
(global-hl-line-mode (- (display-color-cells) 16))   ; Highlight current line everywhere
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

;; if the script has a first line of "#!" then do chmod a+x
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; clickable URLs
(add-hook 'text-mode-hook 'goto-address-mode)
(add-hook 'prog-mode-hook 'goto-address-prog-mode)

;; show trailing whitespaces (source code only)
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; delete trailing whitespaces on save (source code only)
(add-hook 'before-save-hook '(lambda() (when (derived-mode-p 'prog-mode)
                                         (delete-trailing-whitespace))))

;; frame title
(setq frame-title-format '("%b" (buffer-file-name ": %f") " [" (:eval mode-name) "]"))

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
  ;; use fuzzy flx matching by default
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
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
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode 1))

;; even better completion for some functions powered by ivy
(use-package counsel :ensure t
  :config
  (use-package smex :ensure t)       ; automatically used for counsel-M-x
  (ivy-rich-reload)                  ; ensure ivy-rich also applies to counsel functions
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h S" . counsel-info-lookup-symbol)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c J" . counsel-rg)
         ("M-§" . counsel-imenu)
         ("M-y" . counsel-yank-pop)))

;; incremental search powered by ivy
(use-package swiper :ensure t
  :config
  (add-to-list 'ivy-re-builders-alist '(swiper . ivy--regex-plus))
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

;;; tooltips for currently entered command (prefix)
(use-package which-key :ensure t
  :diminish ""
  :config (which-key-mode)
  :custom
  (which-key-add-column-padding 1))

;;; expand region semantically around cursor
(use-package expand-region :ensure t
  :bind ("C-=" . er/expand-region))

;;; magit -- Git interface
(use-package magit :ensure t :defer t
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq exec-path (add-to-list 'exec-path "C:/Program Files (x86)/Git/bin"))
  :bind ("C-x g" . magit-status))

;;; .editorconfig support
(use-package editorconfig :ensure t :defer t
  :diminish ""
  :init
  ;; delay loaded until we visit a non virtual buffer
  (add-hook 'prog-mode-hook (lambda () (when buffer-file-name (editorconfig-mode 1))))
  (add-hook 'text-mode-hook (lambda () (when buffer-file-name (editorconfig-mode 1)))))

;;; Jenkinsfile files support
(use-package groovy-mode :ensure t :defer t)

;;; Dockerfile files support
(use-package dockerfile-mode :ensure t :defer t)

;;; .git{ignore,config,attributes} files support
(use-package gitignore-mode :ensure t :defer t)
(use-package gitconfig-mode :ensure t :defer t)
(use-package gitattributes-mode :ensure t :defer t)

;;; CMakeLists.txt files support
(use-package cmake-mode :ensure t :defer t)

;;; Markdown files support
(use-package markdown-mode :ensure t :defer t)

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
  (when (display-graphic-p)
    (use-package company-quickhelp :ensure t)
    (company-quickhelp-mode 1))
  :bind ("M-p" . company-complete-common)
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.2))


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
(use-package cc-mode
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
  :custom (elpy-rpc-python-command "python3")
  :bind (:map elpy-mode-map
              ("C-c f" . elpy-format-code)
              ;; restore windmove keybinds
              ("M-<up>" . nil)
              ("M-<down>" . nil)
              ("M-<left>" . nil)
              ("M-<right>" . nil)))

;;; .tf files support
(use-package terraform-mode :ensure t :defer t)

;;; .json files support
(use-package json-mode :ensure t :defer t)

;;; .proto files support
(use-package protobuf-mode :ensure t :defer t)

;;; .thrift files support
(use-package thrift :ensure t :defer t
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
(use-package lua-mode :ensure t :defer t)

;;; .yml files support
(use-package yaml-mode :ensure t
  :mode ("\\.yml$" . yaml-mode))

;;; 3D .obj/.mtl files support
(use-package wavefront-obj-mode :ensure t
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
