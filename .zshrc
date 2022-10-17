#!/bin/zsh
# To install, symlink or add the following to ~/.zshrc:
# source ~/dotfiles/.zshrc

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
if [[ "$OSTYPE" == "msys" ]]; then
    alias open='start'
elif [[ "$OSTYPE" == "darwin"* ]]; then
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

# enable completions
# make sure any changes to FPATH are done in ~/.zprofile (ie to install brew's completions)
autoload -Uz compinit bashcompinit
compinit
bashcompinit
# case insensitive TAB completion
zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*'
# partial path completion suggestions
zstyle ':completion:*' list-suffixes
zstyle ':completion:*' expand prefix suffix
# tab completion menu
zstyle ':completion:*' menu select

# g == git
alias gitk='gitk --all'
alias g='git'
alias qg='git'

# dc == docker-compose
if command -v docker-compose >/dev/null 2>&1; then
    alias dc='docker-compose'
fi

# key bindings
bindkey -e  # emacs keybindings
if [[ "$OSTYPE" == "darwin"* ]]; then
    bindkey "\e[1;5D" backward-word    # ctrl-left to move to previous word
    bindkey "\e[1;5C" forward-word     # ctrl-right to move to next word
    bindkey "[D" backward-word         # meta-left to move to previous word
    bindkey "[C" forward-word          # meta-right to move to next word
    bindkey "\e[H" beginning-of-line   # Home as C-a
    bindkey "\e[F" end-of-line         # End as C-e
#else
#    bindkey "\C-h" backward-kill-word  # ctrl-backspace kills chars backward (Ctrl-w)
fi
##bind '"\e[3;5~":kill-word'            # ctrl-del kills chars forward (Alt-d)
##bind '"\M-;":yank-last-arg'           # insert last word of previous line
##bind 'set completion-ignore-case on'  # case-insensitive TAB completion
##bind '"\C-f":menu-complete'           # ctrl-F to cycle through completions
##bind '"\eh":"\C-a\eb\ed\C-y\e#man \C-y\C-m\C-p\C-p\C-a\C-d\C-e"' # poor man's run-help

# Left prompt
# TODO investigate vcs_info more
autoload -Uz vcs_info add-zsh-hook
setopt PROMPT_SUBST                  # enable ${} substitution
export VIRTUAL_ENV_DISABLE_PROMPT=1  # instruct venvs to not fiddle with our prompt
zstyle ":vcs_info:*" enable git
zstyle ":vcs_info:*" formats "(%F{green}%b:%1.7i%f%u%c)"
zstyle ":vcs_info:*" actionformats "(%F{green}%b:%1.7i%f%u%c|%a%m)"
zstyle ":vcs_info:*" branchformat "%b"
zstyle ":vcs_info:*" unstagedstr " %F{red}*%f"
zstyle ":vcs_info:*" stagedstr " %F{green}+%f"
zstyle ":vcs_info:*" patch-format " %n/%a"
zstyle ":vcs_info:*" nopatch-format ""
zstyle ":vcs_info:*" get-revision true
zstyle ":vcs_info:*" check-for-changes true
zstyle ":vcs_info:*" max-exports 1  # TODO what does this do?
add-zsh-hook precmd vcs_info
() {
    local virtualenv='${VIRTUAL_ENV:+[`basename ${VIRTUAL_ENV%%/(.|)venv}`] }'
    local baseprompt='%F{white}%n@%m%f %F{green}%B%5~%b%f '
    local vcsinfo='${vcs_info_msg_0_:+${vcs_info_msg_0_} }'
    local errorcode='%(0?..%F{red}%B$?%b%f )'
    PROMPT="${virtualenv}${baseprompt}${vcsinfo}${errorcode}%(!.#.$) "
}

# Right prompt
export RPROMPT='%*'

# TODO iterm2 shell integration with zsh? I think it works already?
### Modify the prompt further for iTerm2 integration (do not fiddle with prompt after this!)
##if [[ -f "${HOME}/.iterm2_shell_integration.bash" ]]; then
##    source "${HOME}/.iterm2_shell_integration.bash"
##fi

# share history and don't clutter it too much
HISTSIZE=10000
SAVEHIST=12000
setopt share_history            # share history accross sessions
setopt append_history           # append to history file at exit
setopt hist_no_store            # do not store history related commands
setopt hist_ignore_dups         # do not store duplicates
setopt hist_ignore_all_dups     # do not store duplicates
setopt hist_ignore_space        # do not store lines starting with a space
setopt hist_reduce_blanks       # do not store blank lines
export HISTORY_IGNORE='(cccccc|fetch)*'  # do not store mistaps on yubikeys

### proper defaults
export EDITOR=vim
setopt CORRECT                  # correct typos
setopt CORRECT_ALL              # correct typos everywhere?
##shopt -s checkwinsize         # TODO (bash) adjust to terminal window size
#setopt NO_CASE_GLOB            # TODO (bash) case-insensitive glob
##shopt -s extglob              # TODO (bash) extended pattern matching operators using parens

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
