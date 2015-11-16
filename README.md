# Common setup
    cd ~/
    for f in bash_profile bashrc gitconfig pystartup tmux.conf vimrc
    do
        ln -s ~/dotfiles/$f .$f
    done

    mkdir -p ~/.config
    ln -s ~/dotfiles/bash_completion ~/.config/
    ln -s ~/dotfiles/vimrc .nvimrc

    curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    vim +PlugInstall +qall

# Mac setup

* Chrome
* http://mxcl.github.com/homebrew/
* Brews:
    brew install mobile-shell
    brew install vim --with-lua
