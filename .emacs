;; Emacs 25.1
;;
;; Uses builtin package.el and use-package.
;;
;; To install, simply symlink or add the following to ~/.emacs:
;;
;; ;; Load generic version
;; ;(package-initialize)
;; (load "~/dotfiles/.emacs")

(setq my-init-file-start-time (current-time))

;; set garbace collector threshold to 10MB, considerably speeding up emacs
(setq gc-cons-threshold (* 10 1024 1024))

;; setup a cozy welcome screen
(setq inhibit-startup-message t)     ;; No startup message in minibuffer
(setq initial-scratch-message
      (format (concat ";; Hi there, this is %s\n"
                      ";; you might want to visit me %s\n")
              (replace-regexp-in-string "\n" "" (emacs-version))
              (abbreviate-file-name (or load-file-name buffer-file-name))))
(when (display-graphic-p)
  (modify-frame-parameters nil '((wait-for-wm . nil))) ;; Faster font change (really?)
  (add-to-list 'initial-frame-alist '(height . 48))    ;; Slightly bigger initial frame
  (add-to-list 'initial-frame-alist '(width . 120))    ;; Slightly bigger initial frame
  (load-theme 'wombat)                                 ;; cool dark builtin theme
)

;; setup package repos for M-x package-*
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
;(package-install-selected-packages)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)            ;; log package loading times

;; usual tweaks
(setq ring-bell-function 'ignore)                    ;; NO stupid bell sound
(scroll-bar-mode -1)                                 ;; No scrollbar
(tool-bar-mode -1)                                   ;; No toolbar
(menu-bar-mode -1)                                   ;; No menubar
(fset 'yes-or-no-p 'y-or-n-p)                        ;; 'y/n' instead of 'yes/no'
(setq line-number-mode t)                            ;; Line number in modeline
(setq column-number-mode t)                          ;; Column number in modeline
(display-time)                                       ;; Time in modeline
(setq display-time-24hr-format t)                    ;; 24h time format in modeline
(size-indication-mode t)                             ;; Show size of file in modeline
(setq font-lock-maximum-decoration t)                ;; Unleash a rainbow of colors
(global-font-lock-mode t)                            ;; Enable colors
(show-paren-mode t)                                  ;; Highlight matching paren
(global-hl-line-mode (if (display-graphic-p) 1 0))   ;; Highlight current line everywhere
(auto-image-file-mode t)                             ;; Open images NOT in raw data
(setq scroll-step 3)                                 ;; Scroll lines 3 by 3
(setq next-screen-context-lines 3)                   ;; Page up/down keep 3 lines
(setq mouse-yank-at-point t)                         ;; Mouse copy on text pointer
(setq scroll-preserve-screen-position t)             ;; Page up/down preserve point on screen
(mouse-avoidance-mode 'animate)                      ;; Move the mouse away when cursor approaches !
(mouse-wheel-mode t)                                 ;; Enable mouse-wheel to scroll
(which-function-mode)                                ;; Display current function in modeline
(setq kill-whole-line t)                             ;; Cut whole line when point is at col0
(toggle-uniquify-buffer-names)                       ;; Add parent dirs to buffer names when ambiguous
(set-default 'indicate-empty-lines t)                ;; Mark end of file lines in fringe
(set-default 'indicate-buffer-boundaries 'left)      ;; Mark buffer boundaries in fringe
(setq make-backup-files nil)                         ;; Do not create backup~ files
(setq delete-old-versions t)                         ;; Silently delete old backup~ files
(setq delete-auto-save-files t)                      ;; Delete #autosave# files on save
;(global-auto-revert-mode t)                         ;; Auto reload files when modified
;(dynamic-completion-mode t)

;; forbid moving point into read-only part of the prompt
(plist-put minibuffer-prompt-properties 'point-entered 'minibuffer-avoid-prompt)

;; if the script has a first line of "#!" then do chmod a+x
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; delete trailing whitespaces on save
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; frame title
(setq frame-title-format '("%b" (buffer-file-name ": %f") " [" (:eval mode-name) "]"))

;; windmove -- [M-arrows] to move from window to window
(windmove-default-keybindings 'meta)

;; ido -- [C-x C-f] [C-x b] smart completion
(ido-mode 1)
(setq ido-everywhere t)                 ;; use it for as many file dialogs as possible
(setq ido-enable-flex-matching t)       ;; fuzzy matching
(setq ido-confirm-unique-completion t)  ;; if TAB completes uniquely, wait for RET
(setq ido-use-virtual-buffers t)        ;; remember previously opened files
(setq ido-use-filename-at-point 'guess) ;; enable ffap magic
(setq ido-use-url-at-point 'guess)      ;; enable ffap magic
(with-eval-after-load "ido"
  (add-to-list 'ido-ignore-buffers "^ ")
  (add-to-list 'ido-ignore-buffers "*Messages*")
  (add-to-list 'ido-ignore-buffers "*Buffer")
  (add-to-list 'ido-ignore-buffers "*Completions")
  (add-to-list 'ido-ignore-buffers "^[tT][aA][gG][sS]$")
)

;; swap buffers à la windmove
(use-package buffer-move :ensure t
  :bind (([M-S-up] . buf-move-up)
	 ([M-S-down] . buf-move-down)
	 ([M-S-left] . buf-move-left)
	 ([M-S-right] . buf-move-right)))

;; [M-x] [M-X] idoize execute command
(use-package smex :ensure t
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)))

;; [M-.] idoize imenu
(use-package idomenu :ensure t
  :bind ("M-." . idomenu))

;; [M-x magit] Git interface
(use-package magit :ensure t
  :defer t
  :config
  (setq exec-path (add-to-list 'exec-path "C:/Program Files (x86)/Git/bin")))

;; Dockerfile files support
(use-package dockerfile-mode :ensure t :defer t)

;; .git{ignore,config,attributes} files support
(use-package gitignore-mode :ensure t :defer t)
(use-package gitconfig-mode :ensure t :defer t)
(use-package gitattributes-mode :ensure t :defer t)

;; CMakeLists.txt files support
(use-package cmake-mode :ensure t :defer t)

;; Markdown files support
(use-package markdown-mode :ensure t :defer t)

;; Python IDE
(use-package elpy
  :commands elpy-enable
  :init (with-eval-after-load 'python (elpy-enable))
  :config
  (use-package jedi :ensure t)
  (use-package company-quickhelp :ensure t)
  (company-quickhelp-mode)
  (define-key elpy-mode-map (kbd "M-<up>") nil)      ;; do not...
  (define-key elpy-mode-map (kbd "M-<down>") nil)    ;; ...overwrite...
  (define-key elpy-mode-map (kbd "M-<left>") nil)    ;; ...windmove...
  (define-key elpy-mode-map (kbd "M-<right>") nil)   ;; ...keybinds !
  (setq elpy-rpc-backend "jedi"))

;; .yml files support
(use-package yaml-mode :ensure t
  :mode ("\\.yml$" . yaml-mode))

;; 3D .obj/.mtl files support
(use-package wavefront-obj-mode :ensure t
  :mode (("\\.obj$" . wavefront-obj-mode)
	 ("\\.mtl$" . wavefront-obj-mode)))

;; when console interface
(unless (display-graphic-p)
  (normal-erase-is-backspace-mode -1)                  ;; Fix delete key
  (set-face-foreground 'font-lock-comment-face "red")  ;; Better colors ...
  (set-face-foreground 'font-lock-string-face "green") ;; ... for the terminal
)

; when graphical interface
(when (display-graphic-p)
  (blink-cursor-mode 0)                                ;; static cursor
  (set-face-underline-p 'highlight nil)                ;; fix wombat theme with hl-line
  (set-face-foreground 'highlight nil)                 ;; fix wombat theme with hl-line
)

;; keymaps for console
(unless (display-graphic-p)
  (define-key function-key-map "\e[1;5H" [C-home])
  (define-key function-key-map "\e[1;5F" [C-end])
  (define-key function-key-map "\e[1;5A" [C-up])
  (define-key function-key-map "\e[1;5B" [C-down])
  (define-key function-key-map "\e[1;5D" [C-left])
  (define-key function-key-map "\e[1;5C" [C-right])
  (define-key function-key-map "\e[1;3D" [M-left])
  (define-key function-key-map "\e[1;3C" [M-right])
  (define-key function-key-map "\e[1;3A" [M-up])
  (define-key function-key-map "\e[1;3B" [M-down]))

;; custom keyboard binds
(global-set-key [(control a)] 'mark-whole-buffer)      ;; Select whole buffer
(global-set-key "\C-cu" 'uncomment-region)             ;; Uncomment selected region
(global-set-key "\C-cc" 'comment-region)               ;; Comment selected region
(global-set-key "\C-c\C-c" 'comment-or-uncomment-region);; Toggle comment selected region
(global-set-key [mouse-3] 'imenu)                      ;; Right-click list of functions
(global-set-key [C-M-up] 'scroll-down-line)            ;; Scroll line by line...
(global-set-key [C-M-down] 'scroll-up-line)            ;; ...leaving point in place

;; [C-c "] [C-c <].. Wrap selection/word with paired chars
(defun wrap-selection-or-word-with-chars-around (firstchar &optional secondchar)
  "Inserts chars around selected text or current word"
  (interactive)
  (if (and transient-mark-mode mark-active)
      (let ((e1 (region-beginning)) (e2 (region-end)))
        (kill-region e1 e2)
        (insert firstchar)
        (yank)
        (insert (or secondchar firstchar)))
    (let ((thing (thing-at-point 'word))
          (bounds (bounds-of-thing-at-point 'word)))
      (delete-region (car bounds) (cdr bounds))
      (insert firstchar thing (or secondchar firstchar)))))
(global-set-key "\C-c\"" '(lambda () (interactive)(wrap-selection-or-word-with-chars-around "\"")))
(global-set-key "\C-c\'" '(lambda () (interactive)(wrap-selection-or-word-with-chars-around "\'")))
(global-set-key "\C-c<"  '(lambda () (interactive)(wrap-selection-or-word-with-chars-around "<" ">")))
(global-set-key "\C-c\(" '(lambda () (interactive)(wrap-selection-or-word-with-chars-around "(" ")")))
(global-set-key "\C-c\[" '(lambda () (interactive)(wrap-selection-or-word-with-chars-around "[" "]")))

;; Replace GNU advertising
(defun display-startup-echo-area-message ()
  (message "All done in %.02fs, %s%s"
	   (float-time (time-since my-init-file-start-time))
	   (user-login-name) ". \\o/"))
