# Dotfiles

Some of my dotfiles used on Archlinux, Ubuntu, mingw and WSL on windows.

# Installation

Nothing fancy, simply include from root dotfiles or directly symlink.

## bashrc

``` sh
# load generic config
source ~/dotfiles/.bashrc

# specific config
export TGI=T0106400
```

## emacs

``` elisp
;;; Load generic version
;;(package-initialize)
(load "~/dotfiles/.emacs")

;;; Specific config
(setq user-full-name "First Last")
(setq user-mail-address "whatever@example.com")

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

## vim

``` vim
" include generic config
include ~/dotfiles/.vimrc
" specific config
```
