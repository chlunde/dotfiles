# Common setup
    cd ~/
    for f in bash_profile bashrc gitconfig pystartup tmux.conf vimrc
    do
        ln -s ~/dotfiles/$f .$f
    done

# Mac setup

* Chrome
* http://mxcl.github.com/homebrew/
* Brews:
    brew install mobile-shell
