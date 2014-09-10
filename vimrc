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

augroup vimrc
    au!

    au BufWritePre *.{C,cc,cpp,c,java,h,py} silent! %s/[ \t]\+$//
    au BufRead *.{C,cc,cpp,c,java,h,py} silent! %s/[ \t]\+$//

    " Make sure Vim returns to the same line when you reopen a file.
    au BufReadPost *
                \ if line("'\"") > 0 && line("'\"") <= line("$") |
                \     execute 'normal! g`"zvzz' |
                \ endif
augroup END

if v:version < 703 || (v:version == 703 && !has("patch430"))
    filetype off
endif

set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()


Plugin 'gmarik/Vundle.vim'
if v:version < 703
    Plugin 'tpope/vim-git'
endif
Plugin 'kien/ctrlp.vim'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-unimpaired'
Plugin 'tpope/vim-markdown'
Plugin 'chlunde/vim-signify'
Plugin 'epeli/slimux'
Plugin 'vim-scripts/indentpython.vim'
Plugin 'chlunde/jellybeans.vim'
Plugin 'scrooloose/syntastic'
Plugin 'itchyny/lightline.vim'
Plugin 'Shougo/neocomplete'

Plugin 'Shougo/neosnippet'
Plugin 'Shougo/neosnippet-snippets'

if $GOPATH != ""
    Plugin 'fatih/vim-go'
endif

" Plugin 'majutsushi/tagbar'
" autocmd BufEnter * nested :call tagbar#autoopen(-1)
"
" Plugin 'davidhalter/jedi-vim'
" Plugin 'michaeljsmith/vim-indent-object'
" Plugin 'msanders/snipmate.vim.git'
" Plugin 'marijnh/tern_for_vim'

call vundle#end()


syntax on
filetype plugin indent on

set t_Co=256
colorscheme jellybeans

let mapleader=","
nmap <leader>r :registers<CR>
nmap <leader>m :marks<CR>
nmap <leader>c :Gcommit --verbose<CR>
nmap <leader>w :Gwrite<CR>

nmap <silent> <leader>ev :sp $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

" Source
vnoremap <leader>S y:execute @@<cr>:echo 'Sourced selection.'<cr>
nnoremap <leader>S ^vg_y:execute @@<cr>:echo 'Sourced line.'<cr>

nnoremap Q gqip

nnoremap <leader><space> :noh<cr>

" Keep search matches in the middle of the window.
nnoremap n nzzzv
nnoremap N Nzzzv

" Same when jumping around
nnoremap g; g;zz
nnoremap g, g,zz
nnoremap <c-o> <c-o>zz

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
set wildignore+=.git/objects/*
set wildignore+=target
let g:ctrlp_custom_ignore = -1  "'\v[\/]\.git$'

let g:syntastic_python_checkers=['flake8'] ", 'pep257', 'pylint', 'py3kwarn']

let python_highlight_all = 1

" Autoupdate vimdiff on save
autocmd BufWritePost * if &diff == 1 | diffupdate | endif

let g:signify_vcs_list = ['git']
let g:signify_difftool = 'diff'

let g:go_snippet_engine = "neosnippet"

autocmd FileType go setlocal noexpandtab shiftwidth=4 tabstop=4 softtabstop=4

inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"

imap <expr><TAB> neosnippet#expandable_or_jumpable() ?
            \ "\<Plug>(neosnippet_expand_or_jump)"
            \: pumvisible() ? "\<C-n>" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
            \ "\<Plug>(neosnippet_expand_or_jump)"
            \: "\<TAB>"

autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

let g:neocomplete#enable_at_startup = 1

let g:lightline = {
      \ 'colorscheme': 'jellybeans',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'fugitive', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component': {
      \   'fugitive': '%{exists("*fugitive#head") ? fugitive#head() : ""}',
      \ }}
