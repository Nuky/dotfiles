# Dotfiles

Some of my dotfiles used on Arch, Ubuntu and mingw (from git for windows).

# Installation

Nothing fancy, simply include from root dotfiles or directly symlink.

## bashrc

``` sh
# load generic config
source ~/dotfiles/.bashrc

# specific config
alias emacs="/c/Program\ Files\ \(x86\)/emacs-24.4/bin/emacs.exe"
```

## emacs

``` elisp
;;; Load generic version
;;(package-initialize)
(load "~/dotfiles/.emacs")

;;; Specific config

;;; Generated stuff below !
```

## gitconfig

``` ini
# include generic config
[include]
        path = ~/dotfiles/.gitconfig

# specific config
[user]
        email = yourmail@example.com
        name = Your Name
```
