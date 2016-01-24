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

let &titlestring = substitute(hostname(), '\..*', '', 0) . " " . expand("%:t")
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
set wildignore+=*.pyc

augroup vimrc
    au!

    au BufWritePre *.{C,cc,cpp,c,java,h,py} silent! %s/[ \t]\+$//
    au BufRead *.{C,cc,cpp,c,java,h,py} silent! %s/[ \t]\+$//

    au BufWritePost vimrc,.vimrc,init.vim nested if expand('%') !~ 'fugitive' | source % | endif

    " Make sure Vim returns to the same line when you reopen a file.
    au BufReadPost *
                \ if line("'\"") > 0 && line("'\"") <= line("$") |
                \     execute 'normal! g`"zvzz' |
                \ endif
augroup END

if v:version < 703 || (v:version == 703 && !has("patch430"))
    filetype off
endif

call plug#begin('~/.vim/plugged')

if v:version < 703
    Plug 'tpope/vim-git'
endif
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-markdown'
Plug 'chlunde/vim-signify'
Plug 'vim-scripts/indentpython.vim'
Plug 'pearofducks/ansible-vim', { 'for': ['yml', 'ansible'] }
Plug 'chlunde/jellybeans.vim'
Plug 'scrooloose/syntastic'
Plug 'benekastah/neomake'
Plug 'itchyny/lightline.vim'
" meh, https://github.com/Valloric/ycmd/commit/31c0eb8bda4bce1db49741f39ab22027b53e94b5
let $TRAVIS=1

" cmake gcc-c++ python-devel
Plug 'Valloric/YouCompleteMe', { 'for': ['c', 'cpp', 'py', 'go'], 'do': './install.sh' }
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'

"Plugin 'Shougo/neosnippet'
"Plugin 'Shougo/neosnippet-snippets'

if match($HOSTNAME, "puppet") != -1
    Plug 'rodjek/vim-puppet'
endif

if $GOPATH != ""
    Plug 'fatih/vim-go'
    let g:go_fmt_command = "goimports"
    let g:go_snippet_engine = "neosnippet"
    let g:go_highlight_functions = 1
    let g:go_highlight_methods = 1
    let g:go_highlight_structs = 1
    let g:go_highlight_operators = 1
    let g:go_highlight_build_constraints = 1
endif

call plug#end()


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

nnoremap <leader>" viw<esc>a"<esc>hbi"<esc>lel

"let g:ctrlp_extensions = ['tag']
"let g:ctrlp_root_markers = ['manage.py','.git']
"let g:ctrlp_custom_ignore = -1  "'\v[\/]\.git$'
"
set wildignore+=.git/objects/*
set wildignore+=target
set wildignore+=node_modules

let g:syntastic_python_checkers=['flake8'] ", 'pep257', 'pylint', 'py3kwarn']

let python_highlight_all = 1

" pangloss
let javascript_enable_domhtmlcss=1
let g:jsx_ext_required=0

" Autoupdate vimdiff on save
autocmd! BufWritePost * if &diff == 1 | diffupdate | endif | Neomake
autocmd! BufWritePost *.js exec '!touch %'

let g:signify_vcs_list = ['git']
let g:signify_difftool = 'diff'

let g:neomake_open_list = 1

autocmd FileType go setlocal noexpandtab shiftwidth=4 tabstop=4 softtabstop=4
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

function! s:ctrlp()
    let root = systemlist('git rev-parse --show-toplevel')[0]
    if v:shell_error
        Files
    else
        GitFiles
    endif
endfunction
command! CtrlP call s:ctrlp()

nnoremap <silent> <c-p> :CtrlP<CR>

" http://stackoverflow.com/questions/4292733/vim-creating-parent-directories-on-save
function s:MkNonExDir(file, buf)
    if empty(getbufvar(a:buf, '&buftype')) && a:file!~#'\v^\w+\:\/'
        let dir=fnamemodify(a:file, ':h')
        if !isdirectory(dir)
            call mkdir(dir, 'p')
        endif
    endif
endfunction
augroup BWCCreateDir
    autocmd!
    autocmd BufWritePre * :call s:MkNonExDir(expand('<afile>'), +expand('<abuf>'))
augroup END

let g:lightline = {
      \ 'colorscheme': 'jellybeans',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'fugitive', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component': {
      \   'fugitive': '%{exists("*fugitive#head") ? fugitive#head() : ""}',
      \ }}
