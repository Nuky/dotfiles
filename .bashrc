# To install, symlink or add the following to ~/.bashrc:
# source ~/dotfiles/.bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# proper defaults
alias df='df -hT'
alias du="du -h"
alias ls='ls --color=auto -h'
alias mkdir='mkdir -p -v'
# useful aliases
alias e='emacs -nw'
alias la='ll -a'
alias ll='ls -l'
alias lld='ll -d */'
eval $(dircolors -b) # colors for lsd -(*,*)_
alias lsd='ls -d */'
# frequent typos :D
alias k='l'
alias kl='l'
alias l='ls'
alias lq='la'
alias lk='l'

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
else
    bind '"\C-h":backward-kill-word'  # ctrl-backspace kills chars backward (Ctrl-w)
fi
bind '"\e[3;5~":kill-word'            # ctrl-del kills chars forward (Alt-d)
bind '"\M-;":yank-last-arg'           # insert last word of previous line
bind 'set completion-ignore-case on'  # case-insensitive TAB completion
bind '"\C-f":menu-complete'           # ctrl-F to cycle through completions

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

# proper defaults
export EDITOR=vim

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

# Utility to toggle proxy at thales
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
	    unset http_proxy https_proxy no_proxy HTTP_PROXY HTTPS_PROXY NO_PROXY
	    ;;
	""|on)
	    u="${TGI:-$(id -u -n)}"
	    ;&
	*)
	    u="${u:-$1}"
	    read -s -p "Password for $u: " p
	    echo
	    export {HTTP_PROXY,http_proxy}="http://$u:$p@proxy.theresis.org:80"
	    export {HTTPS_PROXY,https_proxy}="https://$u:$p@proxy.theresis.org:3128"
	    export {NO_PROXY,no_proxy}="localhost,127.0.0.1,.theresis.org"
	    ;;
    esac
}
