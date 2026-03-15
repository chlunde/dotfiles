#!/bin/bash
BASHRCSOURCED=Y

# mind the loading time :-)
# time bash -i -c exit

[ -z "$PS1" ] && return # Not interactive
unset command_not_found_handle

__ctx() {
    local x=$?
    if [[ $x -ne 0 ]]; then
        echo -n "! $x "
    fi

    if [[ -n $AWS_PROFILE ]]; then
        echo -n "[$AWS_PROFILE] "
    fi
}

shorthost_prompt() {
    local GREEN='\[\e[32m\]'
    local RESET='\[\e[0m\]'

    local had_extglob=0
    shopt -q extglob && had_extglob=1
    shopt -s extglob
    local host=${HOSTNAME/%.+([a-z0-9]).no/}
    local host="\u@${host/%.localdomain/} "
    if [ -z "$SSH_CLIENT" ] && [ -z "$SSH_TTY" ]; then
        local host=""
    fi
    ((had_extglob)) || shopt -u extglob

    PROMPT_DIRTRIM=2
    if [[ -f /usr/share/doc/git/contrib/completion/git-prompt.sh ]]; then
        . /usr/share/doc/git/contrib/completion/git-prompt.sh
        # shellcheck disable=SC2034
        GIT_PS1_SHOWDIRTYSTATE="true"
        # shellcheck disable=SC2034
        #GIT_PS1_SHOWUNTRACKEDFILES="true"
        # shellcheck disable=SC2034
        GIT_PS1_SHOWUPSTREAM="true"
    else
        __git_ps1() {
            echo
        }
    fi
    PS1="${host}\$(__ctx)$x${GREEN}\w${RESET}\${_GIT_PS1_CACHE}\\$ "
}

shorthost_prompt

pathmunge() {
    if ! [[ -d $1 ]]; then
        return
    fi

    case ":${PATH}:" in
        *:"$1":*) ;;
        *)
            if [ "$2" = "after" ]; then
                PATH=$PATH:$1
            else
                PATH=$1:$PATH
            fi
            ;;
    esac
}

# Disable XON/XOFF
stty -ixon

pathmunge "$HOME/opt/go/bin"

test -L /sbin || pathmunge /sbin
pathmunge /usr/sbin
pathmunge /local/bin after
pathmunge "$HOME/.local/bin"
pathmunge "$HOME/bin"
pathmunge "$HOME/go/bin"
pathmunge "$HOME/opt/bin"
pathmunge "$HOME/opt/emacs/bin"
pathmunge "$HOME/opt/git/bin"
pathmunge "$HOME/opt/ripgrep"
pathmunge "$HOME/opt/nvim/bin"

if command -v nvim &>/dev/null; then
    export EDITOR=nvim
    alias vim=nvim
else
    export EDITOR=vim
fi

alias vi="\$EDITOR"
export ALTERNATE_EDITOR=""

HISTTIMEFORMAT='%FT%T '

_update_prompt() {
    _GIT_PS1_CACHE=$(__git_ps1 " (%s)")
}
# "infinite" history
PROMPT_COMMAND='history -a; _update_prompt'
HISTSIZE=100000

shopt -s checkwinsize

alias m='make'
case $MACHTYPE in
    *linux*)
        alias ls='ls -FX --color=auto'
        ;;
    *)
        alias ls="ls -hFG"
        ;;
esac

if [[ -x ~/bin/fzf ]] && [[ -z $INSIDE_EMACS ]]; then
    # alt-c: project switcher (see bin/fzf-proj)
    bind '"\ec": " \C-e\C-u$(__fzf-proj)\e\C-e\C-m"'
fi

#shopt -s progcomp
#[ -f /etc/bash_completion.d/git ] && source /etc/bash_completion.d/git

# lazy load bash completions (avoid startup cost)
_lazy_complete() {
    # shellcheck disable=SC1090
    source "$HOME/.local/share/bash-completion/$1"
}

__update-completions() {
    local -r compdir="$HOME/.local/share/bash-completion"
    test -d "$compdir" || mkdir -p "$compdir"

    local sentinel="$compdir/.loaded"
    if [[ /usr/share/bash-completion/bash_completion -nt $sentinel ]]; then
        . /usr/share/bash-completion/bash_completion
        touch "$sentinel"
    elif [[ /opt/homebrew/share/bash-completion/bash_completion -nt $sentinel ]]; then
        . /opt/homebrew/share/bash-completion/bash_completion
        touch "$sentinel"
    fi

    local bin
    for bin in kubectl gh kind; do
        [[ -f $compdir/$bin ]] && complete -F _lazy_complete "$bin"
    done

    # regenerate stale completions in background
    (
        for bin in kubectl gh kind; do
            local p
            p=$(builtin type -P "$bin" 2>/dev/null) || continue
            if [[ ! -f $compdir/$bin || $compdir/$bin -ot $p ]]; then
                case $bin in
                    kubectl) kubectl completion bash >"$compdir/$bin" ;;
                    gh) gh completion -s bash >"$compdir/$bin" ;;
                    kind) kind completion bash >"$compdir/$bin" ;;
                esac
            fi
        done
    ) &
    disown
}

__update-completions

alias k=kubectl
_lazy_complete_k() {
    source "$HOME/.local/share/bash-completion/kubectl" 2>/dev/null
    complete -o default -F __start_kubectl k
    __start_kubectl "$@"
}
complete -o default -F _lazy_complete_k k

# Set terminal title automatically via PS0 (bash 5.3+ function substitution)
# Ghostty handles titles natively via shell integration — skip this overhead
__set_title() {
    local cmd
    cmd=$(HISTTIMEFORMAT= builtin history 1)
    cmd="${cmd#*[[:digit:]][* ] }"
    # keep at most 3 words, drop shell metacharacters
    local word count=0 result=
    for word in $cmd; do
        [[ $word == *[?\|'&''<''>']* ]] && break
        result+="${result:+ }$word"
        ((++count >= 3)) && break
    done
    local pwd="${PWD##*/}"
    [[ $pwd == "$USER" ]] && pwd="~"
    if [[ -n $TMUX ]]; then
        tmux rename-window "$pwd: $result"
    else
        printf '\033]0;%s: %s\007' "$pwd" "$result"
    fi
}

_last=
while IFS= read -r _last; do :; done <"$HOME/.bashrc"
if [[ $_last == *opt/etc/shrc* ]]; then
    sed --follow-symlinks -i '$d' "$HOME/.bashrc"
fi
unset _last
[ -f ~/.bashrc.local ] && source ~/.bashrc.local

if [[ -z $GHOSTTY_RESOURCES_DIR ]] && ((BASH_VERSINFO[0] > 5 || (BASH_VERSINFO[0] == 5 && BASH_VERSINFO[1] >= 3))); then
    PS0='${ __set_title; }' # bash 5.3 function substitution
fi
return
