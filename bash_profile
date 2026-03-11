#!/bin/bash
[ -e ~/.bashrc ] && source ~/.bashrc

ulimit -c 262144
test -f "$HOME/.cargo/env" && . "$HOME/.cargo/env"
