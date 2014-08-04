[ -z "$PS1" ] && return # Not interactive
unset command_not_found_handle
trap 'echo -ne "\033]2;$(HISTTIMEFORMAT="" history 1 | sed "s/^[ ]*[0-9]*[ ]*//g")\007"' DEBUG

shorthost_prompt() {
    local opt=$(shopt -p extglob)
    shopt -s extglob
    local host=${HOSTNAME/%.+([a-z0-9]).no/}
    export PS1="\u@${host} \W\\$ "
    eval "$opt"
}

shorthost_prompt

pathmunge () {
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

if [ -d /usr/local/go/bin ]
then
    export GOROOT=/usr/local/go/
    export GOPATH=$HOME/go

    pathmunge /usr/local/go/bin after
    pathmunge $GOPATH/bin after

    . /usr/local/go/misc/bash/go
fi

test -L /sbin || pathmunge /sbin
pathmunge /usr/sbin

pathmunge /local/bin after
pathmunge $HOME/bin

[ -d /opt/vim74/bin ] && export PATH=/opt/vim74/bin:$PATH
[ -d /opt/python27/bin ] && export PATH=/opt/python27/bin:$PATH

export EDITOR=vim

export MAKEFLAGS='-j6'
[ -e ~/.pystartup ] && export PYTHONSTARTUP=~/.pystartup

HISTTIMEFORMAT='%FT%T '

wgets() {
    local H='--header'
    wget $H='Accept-Language: en-us,en;q=0.5' $H='Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' $H='Connection: keep-alive' -U 'Mozilla/5.0 (Windows NT 5.1; rv:10.0.2) Gecko/20100101 Firefox/10.0.2' --referer=http://www.google.com/ "$@";
}

# avoid vim vim foo.c / less vim foo.log etc
less() { case $1 in less|vim) shift ;; esac; command $FUNCNAME ${1+"$@"}; }
vim() { case $1 in less|vim) shift ;; esac; command $FUNCNAME ${1+"$@"}; }

alias m='make'
case $(uname -s) in
    Darwin)
        alias ls="ls -hFG"
        ;;
    *)
        alias ls='ls -FX --color=auto'
        ;;
esac
alias grep='grep --color=tty -d skip'
alias grpe='grep --color=tty -d skip'

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

[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion
# }}}
#source ~/.git-completion.sh

