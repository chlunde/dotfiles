[ -z "$PS1" ] && return # Not interactive
unset command_not_found_handle

# Disable XON/XOFF
stty -ixon

export PATH=/home/chlunde/archive/tmux:$PATH
export MAKEFLAGS='-j6'

HISTTIMEFORMAT='%FT%T '

wgets() {
    local H='--header'
    wget $H='Accept-Language: en-us,en;q=0.5' $H='Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' $H='Connection: keep-alive' -U 'Mozilla/5.0 (Windows NT 5.1; rv:10.0.2) Gecko/20100101 Firefox/10.0.2' --referer=http://www.google.com/ "$@";
}

# avoid vim vim foo.c / less vim foo.log etc
less() { case $1 in less|vim) shift ;; esac; command $FUNCNAME ${1+"$@"}; }
vim() { case $1 in less|vim) shift ;; esac; command $FUNCNAME ${1+"$@"}; }

alias m='make'
alias ls='ls -FX --color=auto'
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
#source ~/.git-completion.sh
