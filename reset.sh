#!/bin/bash

set -e

bak=backup.$(date +%s)
mkdir -p "$bak"
dconf dump / > "$bak/dconf.ini"

dconf reset -f /

dconf load / < desktop/super-return-new-terminal.dconf
dconf load / < desktop/keyboard-us-no.dconf
dconf load / < desktop/clock-date.dconf
dconf load / < desktop/org-gnome-terminal.dconf

code --install-extension golang.go \
	 --install-extension Vue.volar \
	 --install-extension esbenp.prettier-vscode \
	 --install-extension ms-vsliveshare.vsliveshare
