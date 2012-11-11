[ -z "$PS1" ] && return # Not interactive
unset command_not_found_handle

stty -ixon

export PATH=/home/chlunde/archive/tmux:$PATH
export MAKEFLAGS='-j6'

HISTTIMEFORMAT='%FT%T '

wgets() {
    local H='--header'
    wget $H='Accept-Language: en-us,en;q=0.5' $H='Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' $H='Connection: keep-alive' -U 'Mozilla/5.0 (Windows NT 5.1; rv:10.0.2) Gecko/20100101 Firefox/10.0.2' --referer=http://www.google.com/ "$@";
}

less() { case $1 in less) shift ;; esac; command $FUNCNAME ${1+"$@"}; }
vim() { case $1 in vim) shift ;; esac; command $FUNCNAME ${1+"$@"}; }

alias m='make'
alias ls='ls -FX --color=auto'
alias grep='grep --color=tty -d skip'
alias grpe='grep --color=tty -d skip'

a() {
    dest=~/archive/`date +%Y/%m/%d`
    mkdir -p "$dest"
    mv -i -t "$dest" "$@"
}

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

#source ~/.git-completion.sh
