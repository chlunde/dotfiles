#!/bin/bash

[ -z "$PS1" ] && return # Not interactive
unset command_not_found_handle

shorthost_prompt() {
    local opt=$(shopt -p extglob)
    shopt -s extglob
    local host=${HOSTNAME/%.+([a-z0-9]).no/}
    local host=${host/%.localdomain/}
    eval "$opt"

    PROMPT_DIRTRIM=2
    PS1="\u@${host} \w\\$ "
}

shorthost_prompt

pathmunge () {
    if ! [[ -d "$1" ]]
    then
        return
    fi

    case ":${PATH}:" in
        *:"$1":*)
            ;;
        *)
            if [ "$2" = "after" ] ; then
                PATH=$PATH:$1
            else
                PATH=$1:$PATH
            fi
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
test -d /opt/rh/rh-git218 && source scl_source enable rh-git218

alias vi=vim
export EDITOR=vim
export ALTERNATE_EDITOR=""
alias e='emacsclient -t'

em() { emacsclient -a 'emacs' -n "$@" 2>/dev/null; }

[ -e ~/.pystartup ] && export PYTHONSTARTUP=~/.pystartup

HISTTIMEFORMAT='%FT%T '

wgets() {
    local H='--header'
    wget $H='Accept-Language: en-us,en;q=0.5' $H='Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' $H='Connection: keep-alive' -U 'Mozilla/5.0 (Windows NT 5.1; rv:10.0.2) Gecko/20100101 Firefox/10.0.2' --referer=http://www.google.com/ "$@";
}

# avoid vim vim foo.c / less vim foo.log etc
less() { case $1 in less|vim) shift ;; esac; command "$FUNCNAME" ${1+"$@"}; }
vim() { case $1 in less|vim) shift ;; esac; command "$EDITOR" ${1+"$@"}; }

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
    while [[ "$1" = *...* ]]; do
        local new="${1/\.\.\./../..}"; shift
        set "$new" "$@"
    done
 
    if [ "$2" ]; then
        local old="$1"; shift
        local new="$1"; shift
        builtin cd "${opt[@]}" "${PWD/$old/$new}" "$@"
    elif [ -f "$1" ]; then
        local new=$(dirname "$1"); shift
        builtin cd "${opt[@]}" "$new" "$@"
    else
        builtin cd "${opt[@]}" "$@"
    fi
}

cover() {
    local t=$(mktemp -t cover.XXXXXXXXX)
    go test $COVERFLAGS -coverprofile=$t $@ && go tool cover -func=$t && unlink $t
}

if [[ -x ~/bin/fzf ]] && [[ -z "$INSIDE_EMACS" ]]
then

    __fzf_proj__() {
        local dir
        dir=$( (command find -L /git ~/src ~/go/src/ -maxdepth 3 \( -path '*/\.*' -o -fstype 'dev' -o -fstype 'proc' \) -prune \
            -o -type d -print 2> /dev/null; command find -L ~/ -maxdepth 1 -type d 2> /dev/null) | fzf +m) && printf 'cd %q' "$dir"
    }
    # alt-c
    bind '"\ec": " \C-e\C-u$(__fzf_proj__)\e\C-e\C-m"'

    __fzf_shell_in_container__() {
        local container
        container=$(docker ps -a | sed 1d | fzf +m | cut -d' ' -f1) && printf 'docker exec -it %q /bin/bash' "$container"
    }
    # alt-e
    bind '"\ee": " \C-e\C-u$(__fzf_shell_in_container__)\e\C-e\C-m"'
fi

#shopt -s progcomp
#[ -f /etc/bash_completion.d/git ] && source /etc/bash_completion.d/git

[ -f ~/.bashrc.local ] && source ~/.bashrc.local
