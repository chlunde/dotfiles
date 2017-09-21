# Common setup
    cd ~/
    for f in bash_profile bashrc pystartup tmux.conf vimrc emacs
    do
        test -f ~/.$f && mv ~/.$f ~/.${f}.OLD
        ln -s ~/dotfiles/$f .$f
    done

    for f in gitconfig
    do
        test -f ~/.$f || cp ~/dotfiles/$f .$f
    done

    mkdir -p ~/.config ~/.vim
    ln -s ~/.vim ~/.config/nvim
    ln -s ~/dotfiles/vimrc ~/.config/nvim/init.vim

    curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    vim +PlugInstall +qall

    mkdir -p ~/bin
    ln -s ~/dotfiles/bin/go-tool-install ~/bin/
