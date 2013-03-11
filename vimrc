" $Id: .vimrc,v 1.1 2005/02/10 05:14:17 chlunde Exp chlunde $

syntax on
set scrolloff=2
set tabstop=8
set softtabstop=4
set shiftwidth=4
set shiftround
"set textwidth=78
"set backupdir=~/backup
"au BufWritePre * let &bex = '~' . strftime("%Y-%m-%dT%H-%M-%S")
set expandtab
set incsearch
set hlsearch
set ignorecase
set smartcase
"set autoindent
set showcmd
set showmatch
set autowrite
set nocompatible
set ruler
set laststatus=2 " always
set noerrorbells
set visualbell
set title
if &term == "screen"
    set t_ts=]2;
    set t_fs=\
endif
set wildmenu
"set cursorline
set cinoptions=:0,g0,(0,j1
filetype indent on
"set mouse=a
"highlight CursorLine term=bold cterm=bold gui=bold ctermfg=7

let &titlestring = expand("%:t")
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
"set wildignore+=*.pyc

autocmd BufWritePre *.{C,cc,cpp,c,java,h} silent! %s/[ \t]\+$//
autocmd BufRead *.{C,cc,cpp,c,java,h} silent! %s/[ \t]\+$//

map <Down> gj
map <Up>   gk

" Insert timestamp
iab  ymdhms   <c-r>=strftime("%Y-%m-%d %H:%M:%S")<cr> 

nnoremap <leader><space> :noh<cr>

map  <Esc>:tabnew<CR>
map <F1> 1gt
map <F2> 2gt
map <F3> 3gt
map <F4> 4gt
map <F5> 5gt
map <F6> 6gt
map <F7> 7gt
map <F8> 8gt
map <F9> 9gt
map <F10> 10gt
imap <F1> <Esc>1gta
imap <F2> <Esc>2gta
imap <F3> <Esc>3gta
imap <F4> <Esc>4gta
imap <F5> <Esc>5gta
imap <F6> <Esc>6gta
imap <F7> <Esc>7gta
imap <F8> <Esc>8gta
imap <F9> <Esc>9gta
imap <F10> <Esc>10gta

filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'kien/ctrlp.vim'
"Bundle 'tomasr/molokai'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-speeddating'
Bundle 'tpope/vim-unimpaired'
Bundle 'tpope/vim-markdown'
Bundle 'michaeljsmith/vim-indent-object'
"Bundle 'nvie/vim-pyunit'
Bundle 'nvie/vim-flake8'
Bundle 'msanders/snipmate.vim.git'
Bundle 'chlunde/slimux'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'vim-scripts/indentpython.vim'
Bundle 'nanotech/jellybeans.vim'
if v:version < 703
    Bundle 'tpope/vim-git'
endif

if filereadable($HOME . '/tmux/examples/tmux.vim')
    execute "source " . $HOME . '/tmux/examples/tmux.vim'
endif

filetype plugin indent on
set statusline=%<%f\ %h%m\ %{fugitive#statusline()}%r%=%k[%{(&fenc\ ==\ \"\"?&enc:&fenc).(&bomb?\",BOM\":\"\")}][U+%04B]\ %-12.(%l,%c%V%)\ %P 

set t_Co=256
set background=dark
colorscheme jellybeans

" Bundle 'godlygeek/tabular'
" Bundle 'sjl/gundo.vim'
" Bundle 'Raimondi/delimitMate'
" Bundle 'kana/vim-fakeclip'
" https://github.com/vim-scripts/Rainbow-Parenthsis-Bundle
" SNIPMATE http://nvie.com/posts/how-i-boosted-my-vim/
"
" C-x C-o
fun! PyCheck()
    setlocal makeprg=python\ -c\ \"import\ py_compile;\ py_compile.compile(r'%')\"
    setlocal efm=%.%#:\ ('%m'\\,\ ('%f'\\,\ %l\\,\ %c\\,%.%#
    silent! make
    redraw!
    if !empty(getqflist())
	clist
    else
	echo 'OK'
    endif
endfun

nnoremap ; :

map <Up> <nop>
map <Down> <nop>
map <Left> <nop>
map <Right> <nop>

" Easy window navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

map <C-u> maviwU`a

let mapleader=","
map ,p :call PyCheck()<CR>
map ,r :registers<CR>
map ,m :marks<CR>
nmap <silent> <leader>ev :sp $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

map <C-c><C-c> :SlimuxREPLSendLine<CR>
vmap <C-c><C-c> :SlimuxREPLSendSelection<CR>
nnoremap <leader>" viw<esc>a"<esc>hbi"<esc>lel
let g:ctrlp_extensions = ['tag']
let g:ctrlp_root_markers = ['manage.py','.git']
