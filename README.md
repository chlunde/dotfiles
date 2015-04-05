# Common setup
    cd ~/
    for f in bash_profile bashrc gitconfig pystartup tmux.conf vimrc
    do
        ln -s ~/dotfiles/$f .$f
    done

    ln -s ~/dotfiles/vimrc .nvimrc

    git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
    vim +PluginInstall +qall

# Mac setup

* Chrome
* http://mxcl.github.com/homebrew/
* Brews:
    brew install mobile-shell
    brew install vim --with-lua
