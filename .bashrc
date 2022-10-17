#!/bin/bash
# To install, symlink or add the following to ~/.bashrc:
# source ~/dotfiles/.bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# proper defaults
alias df='df -h'
alias du='du -h'
if [[ "$OSTYPE" == "darwin"* ]]; then
    alias ls='ls -Gh'
else
    alias ls='ls --color=auto -h'
fi
alias mkdir='mkdir -p -v'
# useful aliases
alias e='emacs -nw'
alias ll='ls -l'
alias la='ll -a'
alias lld='ll -d */'
if command -v dircolors >/dev/null 2>&1; then
    eval $(dircolors -b) # colors for lsd -(*,*)_
fi
alias lsd='ls -d */'
if [[ "$OSTYPE" == "darwin"* ]]; then
    :
elif (( EUID )); then
    alias open='xdg-open'
fi
alias magit='emacs -nw --eval "(progn (magit-status) (delete-other-windows))"'
# frequent typos :D
alias k='l'
alias kl='l'
alias l='ls'
alias lq='la'
alias lk='l'
# workarounds
alias sudo="sudo " # enable alias expansion of sudoified command

# enable loads of completions (bash-completion)
if [[ -f /usr/share/bash-completion/bash_completion ]]; then
    source /usr/share/bash-completion/bash_completion
elif [[ -f /usr/local/etc/bash_completion ]]; then
    source /usr/local/etc/bash_completion
elif [[ -f /opt/homebrew/etc/bash_completion ]]; then
    source /opt/homebrew/etc/bash_completion
fi

# g == git
if [[ -f /usr/share/git/completion/git-completion.bash ]]; then
    source /usr/share/git/completion/git-completion.bash
elif [[ -f /usr/share/bash-completion/completions/git ]]; then
    source /usr/share/bash-completion/completions/git
elif [[ -f /usr/local/etc/bash_completion.d/git-completion.bash ]]; then
    source /usr/local/etc/bash_completion.d/git-completion.bash
elif [[ -f /opt/homebrew/etc/bash_completion.d/git-completion.bash ]]; then
    source /opt/homebrew/etc/bash_completion.d/git-completion.bash
fi
alias gitk='gitk --all'
alias g='git' && __git_complete g __git_main
alias qg='git' && __git_complete qg __git_main # typo..

# dc == docker-compose
if command -v docker-compose >/dev/null 2>&1; then
    alias dc='docker-compose' && complete -F _docker_compose dc
fi

# key bindings
if [[ "$OSTYPE" == "darwin"* ]]; then
    bind '"\e[1;5D":backward-word'    # ctrl-left to move to previous word
    bind '"\e[1;5C":forward-word'     # ctrl-right to move to next word
    bind '"\e[1;3D":backward-word'    # meta-left to move to previous word
    bind '"\e[1;3C":forward-word'     # meta-right to move to next word
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
elif [[ -f /usr/local/etc/bash_completion.d/git-prompt.sh ]]; then
    source /usr/local/etc/bash_completion.d/git-prompt.sh
elif [[ -f /opt/homebrew/etc/bash_completion.d/git-prompt.sh ]]; then
    source /opt/homebrew/etc/bash_completion.d/git-prompt.sh
fi
if command -v __git_ps1 >/dev/null 2>&1; then
    __my_prompt_command() {
        local EXIT="$?"
        local GREEN="\[\e[1;32m\]"
        local LGRAY="\[\e[0;37m\]"
        local LRED="\[\e[0;91m\]"
        local NONE="\[\e[0m\]"
        local ps1_pre="${VIRTUAL_ENV:+[`basename ${VIRTUAL_ENV/%\/?(.)venv/}`] }${LGRAY}\u@\h${NONE} ${GREEN}\w${NONE}"
        local ps1_post=" ${NONE}\\\$${NONE} "
        [ "${EXIT}" != 0 ] && ps1_post=" ${LRED}${EXIT}${NONE}${ps1_post}"
        __git_ps1 "${ps1_pre}" "${ps1_post}"
    }
    export PROMPT_COMMAND=__my_prompt_command
fi

# In addition, if you set GIT_PS1_SHOWDIRTYSTATE to a nonempty
# value, unstaged (*) and staged (+) changes will be shown next
# to the branch name.  You can configure this per-repository
# with the bash.showDirtyState variable, which defaults to true
# once GIT_PS1_SHOWDIRTYSTATE is enabled.
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWSTASHSTATE=1
#export GIT_PS1_SHOWUNTRACKEDFILES=1  # this one can be very slow
export GIT_PS1_SHOWUPSTREAM="auto"
export GIT_PS1_SHOWCOLORHINTS=1

# Modify the prompt further for iTerm2 integration (do not fiddle with prompt after this!)
if [[ -f "${HOME}/.iterm2_shell_integration.bash" ]]; then
    source "${HOME}/.iterm2_shell_integration.bash"
fi

# don't clutter history too much
export HISTCONTROL=ignoreboth
export HISTIGNORE='history*:cccccc*'
export HISTSIZE=1000
export HISTFILESIZE=2000

# proper defaults
export EDITOR=vim
shopt -s histappend             # do not overwrite history file
shopt -s checkwinsize           # adjust to terminal window size
shopt -s extglob                # extended pattern matching operators using parens
shopt -s globstar 2&>/dev/null  # enable ** recursive wildcard (bash >4.0)

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
command -v pip >/dev/null 2>&1 && alias gpip='PIP_REQUIRE_VIRTUALENV=false pip'
command -v pip2 >/dev/null 2>&1 && alias gpip2='PIP_REQUIRE_VIRTUALENV=false pip2'
command -v pip3 >/dev/null 2>&1 && alias gpip3='PIP_REQUIRE_VIRTUALENV=false pip3'

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# useful functions

jwtdecode ()
{
    jq -R 'split(".")[0,1] | gsub("-";"+") | gsub("_";"/") | @base64d | fromjson' <<< "${1}"
}
