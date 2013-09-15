syntax on
set scrolloff=2
set softtabstop=4
set shiftwidth=4
set shiftround
set expandtab
set incsearch
set hlsearch
set ignorecase
set smartcase
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
"set mouse=a

"let &titlestring = expand("%:t")
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
set wildignore+=*.pyc

autocmd BufWritePre *.{C,cc,cpp,c,java,h,py} silent! %s/[ \t]\+$//
autocmd BufRead *.{C,cc,cpp,c,java,h,py} silent! %s/[ \t]\+$//

if v:version < 703 || (v:version == 703 && !has("patch430"))
    filetype off
endif

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
if v:version < 703
    Bundle 'tpope/vim-git'
endif
Bundle 'kien/ctrlp.vim'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-unimpaired'
Bundle 'tpope/vim-markdown'
Bundle 'chlunde/vim-signify'
Bundle 'epeli/slimux'
Bundle 'vim-scripts/indentpython.vim'
Bundle 'chlunde/jellybeans.vim'
Bundle 'scrooloose/syntastic'
Bundle 'itchyny/lightline.vim'

" Bundle 'majutsushi/tagbar'
" autocmd BufEnter * nested :call tagbar#autoopen(-1)
"
" Bundle 'davidhalter/jedi-vim'
" Bundle 'michaeljsmith/vim-indent-object'
" Bundle 'msanders/snipmate.vim.git'
" Bundle 'marijnh/tern_for_vim'

filetype plugin indent on

set t_Co=256
colorscheme jellybeans

let mapleader=","
nmap <leader>r :registers<CR>
nmap <leader>m :marks<CR>
nmap <silent> <leader>ev :sp $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

nnoremap <leader><space> :noh<cr>

map <Up> <nop>
map <Down> <nop>
map <Left> <nop>
map <Right> <nop>

" Easy window navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

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

map <C-c><C-c> :SlimuxREPLSendLine<CR>
vmap <C-c><C-c> :SlimuxREPLSendSelection<CR>
nnoremap <leader>" viw<esc>a"<esc>hbi"<esc>lel

let g:ctrlp_extensions = ['tag']
let g:ctrlp_root_markers = ['manage.py','.git']

let g:syntastic_python_checkers=['flake8'] ", 'pep257', 'pylint', 'py3kwarn']

let python_highlight_all = 1

let g:signify_vcs_list = ['git']
let g:signify_difftool = 'diff'

let g:lightline = {
      \ 'colorscheme': 'jellybeans',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'fugitive', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component': {
      \   'fugitive': '%{exists("*fugitive#head") ? fugitive#head() : ""}',
      \ }}
