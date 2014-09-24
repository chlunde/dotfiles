#!/bin/bash
[ -e ~/.bashrc ] && source ~/.bashrc

case $(uname -s) in
    Darwin)
        # The default is "UTF-8 (without en_US), breaks some apps
        export LC_ALL=en_US.UTF-8
        ;;
esac

ulimit -c 262144
