#!/bin/bash

# mind the loading time :-)
#
# time bash -i -c exit

[ -z "$PS1" ] && return # Not interactive
unset command_not_found_handle

__ctx() {
    local x=$?
    if [[ $x -ne 0 ]]; then
        echo -n "! $x "
    fi

    if [[ -n $AWS_ACCESS_KEY_ID ]]; then
        echo -n "[$AWS_ACCESS_KEY_ID] "
    fi

    if [[ -n $AWS_PROFILE ]]; then
        echo -n "[$AWS_PROFILE] "
    fi
}

shorthost_prompt() {
    local GREEN="\[$(tput setaf 2)\]"
    local RESET="\[$(tput sgr0)\]"

    local opt=$(shopt -p extglob)
    shopt -s extglob
    local host=${HOSTNAME/%.+([a-z0-9]).no/}
    local host="\u@${host/%.localdomain/} "
    if [[ $host == *client* ]]; then
        local host=""
    fi
    eval "$opt"

    PROMPT_DIRTRIM=2
    if [[ -f /usr/share/doc/git/contrib/completion/git-prompt.sh ]]; then
        . /usr/share/doc/git/contrib/completion/git-prompt.sh
        # shellcheck disable=SC2034
        GIT_PS1_SHOWDIRTYSTATE="true"
        # shellcheck disable=SC2034
        GIT_PS1_SHOWUNTRACKEDFILES="true"
        # shellcheck disable=SC2034
        GIT_PS1_SHOWUPSTREAM="true"
    else
        __git_ps1() {
            echo
        }
    fi
    PS1="${host}\$(__ctx)$x${GREEN}\w${RESET}\$(__git_ps1 \" (%s)\")\\$ "
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

if type nvim &>/dev/null; then
    export EDITOR=nvim
else
    export EDITOR=vim
fi

alias vi="\$EDITOR"
export ALTERNATE_EDITOR=""

#alias e='emacsclient -t'
#em() { emacsclient -a 'emacs' -n "$@" 2>/dev/null; }

[ -e ~/.pystartup ] && export PYTHONSTARTUP=~/.pystartup

HISTTIMEFORMAT='%FT%T '

wgets() {
    local H='--header'
    wget $H='Accept-Language: en-us,en;q=0.5' $H='Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' $H='Connection: keep-alive' -U 'Mozilla/5.0 (Windows NT 5.1; rv:10.0.2) Gecko/20100101 Firefox/10.0.2' --referer=http://www.google.com/ "$@"
}

# avoid vim vim foo.c / less vim foo.log etc
less() {
    case $1 in less | vim) shift ;; esac
    command "$FUNCNAME" ${1+"$@"}
}
vim() {
    case $1 in less | vim) shift ;; esac
    command "$EDITOR" ${1+"$@"}
}

# "infinite" history
PROMPT_COMMAND='history -a'
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
alias grep='grep --color=tty -d skip'
alias grpe='grep --color=tty -d skip'
alias grwp='grep --color=tty -d skip'

# archive stuff by date
a() {
    dest=~/archive/$(date +%Y/%m/%d)
    mkdir -p "$dest"
    mv -i -t "$dest" "$@"
}

# Fancy cd from peder.  with CWD=/foo/qa/bar; cd qa prod changes to /foo/prod/bar
cd() {
    local opt=()
    local arg=()
    local split=1

    # first, get all options out of the way
    for i in "$@"; do
        case "$i" in
            --)
                opt=("${opt[@]}" "$i")
                split=
                ;;
            -?)
                [ "$split" ] \
                    && opt=("${opt[@]}" "$i") \
                    || arg=("${arg[@]}" "$i")
                ;;
            *)
                arg=("${arg[@]}" "$i")
                ;;
        esac
    done

    # reset parameters to only non-option parameters
    [ "${#arg[@]}" == 0 ] || set "${arg[@]}"

    # expand ... to ../..
    while [[ $1 == *...* ]]; do
        local new="${1/\.\.\./../..}"
        shift
        set "$new" "$@"
    done

    if [ "$2" ]; then
        local old="$1"
        shift
        local new="$1"
        shift
        builtin cd "${opt[@]}" "${PWD/$old/$new}" "$@"
    elif [ -f "$1" ]; then
        local new=$(dirname "$1")
        shift
        builtin cd "${opt[@]}" "$new" "$@"
    else
        builtin cd "${opt[@]}" "$@"
    fi
}

if [[ -x ~/bin/fzf ]] && [[ -z $INSIDE_EMACS ]]; then

    __fzf_proj__() {
        local dir
        dir=$( (
            command find -L /git ~/src ~/go/src/ -maxdepth 3 \( -path '*/\.*' -o -fstype 'dev' -o -fstype 'proc' \) -prune \
                -o -type d -print 2>/dev/null
            command find -L ~/ -maxdepth 1 -type d 2>/dev/null
        ) | fzf +m) && printf 'cd %q' "$dir"
    }
    # alt-c
    # shellcheck disable=2016
    bind '"\ec": " \C-e\C-u$(__fzf_proj__)\e\C-e\C-m"'
fi

#shopt -s progcomp
#[ -f /etc/bash_completion.d/git ] && source /etc/bash_completion.d/git

# lazy load bash completions (avoid startup cost)
_lazy_complete() {
    # shellcheck disable=SC1090
    source "$HOME/.local/share/bash-completion/$1"
}

__update-completions() {
    local -r compdir="$HOME/.local/share/bash-completion/"
    test -d "$compdir" || mkdir -p "$compdir"

    . /usr/share/bash-completion/bash_completion

    if type kubectl &>/dev/null; then
        if test "$compdir/kubectl" -ot "$(which kubectl)"; then
            kubectl completion bash >"$compdir/kubectl"
        fi
    fi
    complete -F _lazy_complete kubectl

    if type gh &>/dev/null; then
        if test "$compdir/gh" -ot "$(which gh)"; then
            gh completion -s bash >"$compdir/gh"
        fi
    fi
    complete -F _lazy_complete gh

    if type kind &>/dev/null; then
        if test "$compdir/kind" -ot "$(which kind)"; then
            kind completion bash >"$compdir/kind"
        fi
    fi
    complete -F _lazy_complete kind
}

__update-completions

alias k=kubectl
complete -o default -F __start_kubectl k

# Set terminal title manually
t() {
    AUTO_TITLE=0
    if [[ -n $TMUX ]]; then
        tmux rename-window "$(basename "$PWD") $*"
    else
        echo -ne "\033]0;$(basename "$PWD") $*\007"
    fi
}

# Set terminal title automatically
preexec() { :; }
preexec_invoke_exec() {
    [ -n "$COMP_LINE" ] && return                     # do nothing if completing
    [ "$BASH_COMMAND" = "$PROMPT_COMMAND" ] && return # don't cause a preexec for $PROMPT_COMMAND
    [ "$AUTO_TITLE" = 0 ] && return                   # don't set title when title has been set manually
    local this_command
    this_command="$(HISTTIMEFORMAT="" history 1 | sed -e "s/^[ ]*[0-9]*[ ]*//" -e 's/^\([^ ]\+\s\+[^ ]\+\s\+[^ ]\+\).*/\1/' -e 's/[|&<>].*//')"
    pwd="$(basename "$PWD")"
    if [[ $pwd == "$USER" ]]; then
        pwd="~"
    fi
    if [[ -n $TMUX ]]; then
        tmux rename-window "$pwd: $this_command"
    else
        echo -ne "\033]0;$pwd: $this_command\007"
    fi
}
trap 'preexec_invoke_exec' DEBUG

if tail -n 1 "$HOME/.bashrc" | grep -q opt/etc/shrc; then
    sed --follow-symlinks -i '$d' "$HOME/.bashrc"
fi
[ -f ~/.bashrc.local ] && source ~/.bashrc.local
