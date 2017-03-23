#!/bin/bash
# To install, symlink or add the following to ~/.bashrc:
# source ~/dotfiles/.bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# proper defaults
alias df='df -hT'
alias du='du -h'
alias ls='ls --color=auto -h'
alias mkdir='mkdir -p -v'
# useful aliases
alias e='emacs -nw'
alias la='ll -a'
alias ll='ls -l'
alias lld='ll -d */'
eval $(dircolors -b) # colors for lsd -(*,*)_
alias lsd='ls -d */'
if [[ "$OSTYPE" == "msys" ]]; then
    alias open='start'
elif (( EUID )); then
    alias open='xdg-open'
fi
# frequent typos :D
alias k='l'
alias kl='l'
alias l='ls'
alias lq='la'
alias lk='l'
# workarounds
alias sudo="sudo " # enable alias expansion of sudoified command

# g == git
if [[ -f /usr/share/git/completion/git-completion.bash ]]; then
    source /usr/share/git/completion/git-completion.bash
elif [[ -f /usr/share/bash-completion/completions/git ]]; then
    source /usr/share/bash-completion/completions/git
fi
alias gitk='gitk --all'
alias g='git' && __git_complete g __git_main
alias qg='git' && __git_complete qg __git_main # typo..

# key bindings
if [[ "$OSTYPE" == "msys" ]]; then
    bind '"\C-_":backward-kill-word'  # ctrl-backspace kills chars backward (Ctrl-w)
    bind '"\e[1;5D":backward-word'    # ctrl-left to move to previous word
    bind '"\e[1;5C":forward-word'     # ctrl-right to move to next word
else
    bind '"\C-h":backward-kill-word'  # ctrl-backspace kills chars backward (Ctrl-w)
fi
bind '"\e[3;5~":kill-word'            # ctrl-del kills chars forward (Alt-d)
bind '"\M-;":yank-last-arg'           # insert last word of previous line
bind 'set completion-ignore-case on'  # case-insensitive TAB completion
bind '"\C-f":menu-complete'           # ctrl-F to cycle through completions
bind '"\eh":"\C-a\eb\ed\C-y\e#man \C-y\C-m\C-p\C-p\C-a\C-d\C-e"' # poor man's run-help

# change PROMPT_COMMAND (ie: PS1) to support both git and venv
if [[ -f /usr/share/git/completion/git-prompt.sh ]]; then
    source /usr/share/git/completion/git-prompt.sh
elif [[ -f /etc/bash_completion.d/git-prompt ]]; then
    source /etc/bash_completion.d/git-prompt
fi
if command -v __git_ps1 >/dev/null 2>&1; then
    LIGHT_GREEN="\[\033[1;32m\]"
    LIGHT_GRAY="\[\033[0;37m\]"
    COLOR_NONE="\[\e[0m\]"
    export PROMPT_COMMAND='__git_ps1 "${VIRTUAL_ENV:+[`basename $VIRTUAL_ENV`] }${LIGHT_GRAY}\u${COLOR_NONE} ${LIGHT_GREEN}\w${COLOR_NONE}" " ${COLOR_NONE}\\\$${COLOR_NONE} "'
    #PS1='[\u@\h \W]\$ '
fi

# In addition, if you set GIT_PS1_SHOWDIRTYSTATE to a nonempty
# value, unstaged (*) and staged (+) changes will be shown next
# to the branch name.  You can configure this per-repository
# with the bash.showDirtyState variable, which defaults to true
# once GIT_PS1_SHOWDIRTYSTATE is enabled.
# This is too slow on windows :(
if [[ "$OSTYPE" != "msys" ]]; then
    export GIT_PS1_SHOWDIRTYSTATE=1
    export GIT_PS1_SHOWSTASHSTATE=1
    export GIT_PS1_SHOWUNTRACKEDFILES=1
    export GIT_PS1_SHOWUPSTREAM="auto"
fi
export GIT_PS1_SHOWCOLORHINTS=1

# don't clutter history too much
export HISTCONTROL=ignoreboth
export HISTIGNORE='history*'
export HISTSIZE=1000
export HISTFILESIZE=2000

# proper defaults
export EDITOR=vim
shopt -s histappend             # do not overwrite history file
shopt -s checkwinsize           # adjust to terminal window size
shopt -s globstar               # enable ** recursive wildcard

# Colored man pages
if command -v man >/dev/null 2>&1; then
    man() {
        env LESS_TERMCAP_mb=$'\E[01;31m' \
        LESS_TERMCAP_md=$'\E[01;38;5;74m' \
        LESS_TERMCAP_me=$'\E[0m' \
        LESS_TERMCAP_se=$'\E[0m' \
        LESS_TERMCAP_so=$'\E[38;5;246m' \
        LESS_TERMCAP_ue=$'\E[0m' \
        LESS_TERMCAP_us=$'\E[04;38;5;146m' \
        man "$@"
    }
fi

# pip that allows installs outside of a venv (see pip.conf)
command -v pip >/dev/null 2>&1 && alias gpip='PIP_REQUIRE_VIRTUALENV="" pip'

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# Utilities for managing the proxy at Thales

# Set environment proxy variables
# Note: username can be specified, but it will default to TGI if set, or userid.
setproxy()
{
    local u
    local p

    case "$1" in
	help|--help|-h)
	    echo "Usage: setproxy [<username> | on | off | help]"
	    ;;
	off)
	    unset {{http,https,no,all}_proxy,{HTTP,HTTPS,NO,ALL}_PROXY}
	    ;;
	""|on)
	    u="${TGI:-$(id -u -n)}"
	    ;&
	*)
	    u="${u:-$1}"
	    read -s -p "Password for $u: " p
	    echo
	    export {{http,https,all}_proxy,{HTTP,HTTPS,ALL}_PROXY}="http://$u:$p@proxy.theresis.org:80"
	    export {no_proxy,NO_PROXY}="localhost,127.0.0.1,.theresis.org"
	    ;;
    esac
}

# Run a command with proxy variables environment
proxify()
{
    if [[ ! -v http_proxy ]]; then
        local u="${TGI:-$(id -u -n)}"
        local p
        read -s -p "Password for $u: " p
        echo
        env {{http,https,all}_proxy,{HTTP,HTTPS,ALL}_PROXY}="http://$u:$p@proxy.theresis.org:80" "$@"
    else
        "$@"
    fi
}
alias proxify="proxify "     # enable alias expansion
declare -F _command >/dev/null && complete -F _command proxify # enable nested tab completion

# Run a command without proxy variables environment
unproxify()
{
    env --unset={{http,https,no,all}_proxy,{HTTP,HTTPS,NO,ALL}_PROXY} "$@"
}
alias unproxify="unproxify "   # enable alias expansion
declare -F _command >/dev/null && complete -F _command unproxify # enable nested tab completion
