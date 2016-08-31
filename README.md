# Common setup
    cd ~/
    for f in bash_profile bashrc gitconfig pystartup tmux.conf vimrc emacs
    do
        test -f ~/.$f && mv ~/.$f ~/.${f}.OLD
        ln -s ~/dotfiles/$f .$f
    done

    mkdir -p ~/.config ~/.vim
    ln -s ~/dotfiles/bash_completion ~/.config/
    ln -s ~/.vim ~/.config/nvim
    ln -s ~/dotfiles/vimrc ~/.config/nvim/init.vim

    curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    vim +PlugInstall +qall

    ln -s $PWD/bin/go-tool-install ~/bin/
