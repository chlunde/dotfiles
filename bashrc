# -*- mode: sh -*-

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

[ -d /usr/local/go/bin ] && GOROOT=/usr/local/go
[ -d ~/opt/go ] && GOROOT=~/opt/go
[ -d $HOME/goroot ] && GOROOT=$HOME/goroot

if [[ -n "$GOROOT" ]]
then
    export GOROOT
    export GOPATH=$HOME/go

    pathmunge $GOROOT/bin after
    pathmunge $GOPATH/bin after
fi

test -L /sbin || pathmunge /sbin
pathmunge /usr/sbin
pathmunge /local/bin after
pathmunge $HOME/bin
pathmunge $HOME/opt/bin
pathmunge $HOME/opt/emacs/bin
pathmunge $HOME/opt/git/bin
pathmunge /opt/python27/bin

if [[ -f /usr/bin/nvim ]]
then
    alias vi=nvim
    alias vim=nvim
    export EDITOR=nvim
else
    alias vi=vim
    export EDITOR=vim
fi

export MAKEFLAGS='-j6'
[ -e ~/.pystartup ] && export PYTHONSTARTUP=~/.pystartup

HISTTIMEFORMAT='%FT%T '

wgets() {
    local H='--header'
    wget $H='Accept-Language: en-us,en;q=0.5' $H='Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' $H='Connection: keep-alive' -U 'Mozilla/5.0 (Windows NT 5.1; rv:10.0.2) Gecko/20100101 Firefox/10.0.2' --referer=http://www.google.com/ "$@";
}

# avoid vim vim foo.c / less vim foo.log etc
less() { case $1 in less|vim) shift ;; esac; command $FUNCNAME ${1+"$@"}; }
vim() { case $1 in less|vim) shift ;; esac; command $EDITOR ${1+"$@"}; }

# "infinite" history
history -a
HISTSIZE=100000

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
    dest=~/archive/`date +%Y/%m/%d`
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

# My Python virtualenv helpers {{{
# RH 5.x: wget http://pypi.python.org/packages/source/v/virtualenv/virtualenv-1.7.2.tar.gz
# easy_install pip==1.1
create_venv() {
    if [ -n "$1" ]
    then
	~/venv/virtualenv.py ~/venv/$1
    else
	echo "Usage: create_venv [name] # creates ~/venv/<name>, then try venv <Tab>"
    fi
}

venv() {
   local venv=~/venv/$1
   if [ ! -d $venv ]
   then
	echo $venv does not exist
	return 1
   fi

   source $venv/bin/activate
}

_venv_complete() {
    if [ $COMP_CWORD -gt 1 ]
    then
	COMPREPLY=( )
	return 0
    fi

    local cur="${COMP_WORDS[COMP_CWORD]}"
    local envs=$(for x in $(cd ~/venv/; ls -1 -d */); do echo ${x%%/} ; done )
    COMPREPLY=( $(compgen -W "${envs}" -- ${cur}) )

    return 0
}

complete -F _venv_complete venv

# }}}

if [[ -f ~/.fzf.bash ]]
then
    source ~/.fzf.bash

    __fzf_proj__() {
        local dir
        dir=$((command find -L /git ~/src ~/go/src/ -maxdepth 3 \( -path '*/\.*' -o -fstype 'dev' -o -fstype 'proc' \) -prune \
            -o -type d -print 2> /dev/null; command find -L ~/ -maxdepth 1 -type d)| $(__fzfcmd) +m) && printf 'cd %q' "$dir"
    }
    # alt-c
    bind '"\ec": " \C-e\C-u$(__fzf_proj__)\e\C-e\er\C-m"'

    __fzf_shell_in_container__() {
        local container
        container=$(docker ps -a | sed 1d | $(__fzfcmd) +m | cut -d' ' -f1) && printf 'docker exec -it %q /bin/bash' "$container"
    }
    # alt-e
    bind '"\ee": " \C-e\C-u$(__fzf_shell_in_container__)\e\C-e\er\C-m"'
fi

shopt -s progcomp
[ -f /etc/bash_completion.d/git ] && source /etc/bash_completion.d/git

[ -f ~/.bashrc.local ] && source ~/.bashrc.local
