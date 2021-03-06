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

## pip

``` sh
ln -s ~/dotfiles/config/pip/pip.conf ~/.config/pip/pip.conf
```

On windows, copy to `%APPDATA%\pip\pip.ini`

## tmux

``` sh
ln -s ~/dotfiles/.tmux.conf ~/.tmux.conf
```

## vim

``` vim
" include generic config
source ~/dotfiles/.vimrc
" specific config
```
