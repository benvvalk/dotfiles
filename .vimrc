"------------------------------------------------------------
" Vundle settings
"------------------------------------------------------------

" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just
" :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to
"auto-approve removal

" be iMproved, required
set nocompatible
filetype off

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-repeat'
Plugin 'easymotion/vim-easymotion'
Plugin 'scrooloose/nerdtree'
Plugin 'vimscripts/taglist.vim'
Plugin 'majutsushi/tagbar'
Plugin 'kien/ctrlp.vim'
Plugin 'justinmk/vim-sneak'
Plugin 'kana/vim-textobj-user'
Plugin 'tpope/vim-surround'
Plugin 'kana/vim-textobj-entire'
Plugin 'kana/vim-textobj-indent'
Plugin 'kana/vim-textobj-line'
Plugin 'sgur/vim-textobj-parameter'
Plugin 'jeetsukumaran/vim-indentwise'
Plugin 'MarcWeber/vim-addon-mw-utils'
Plugin 'tomtom/tlib_vim'
Plugin 'garbas/vim-snipmate'
Plugin 'honza/vim-snippets'
Plugin 'tpope/vim-vinegar'
call vundle#end()

filetype plugin indent on
filetype on

"------------------------------------------------------------
" file types by extension
"------------------------------------------------------------

au BufNewFile,BufRead *.py set filetype=python
au BufNewFile,BufRead *.rb set filetype=ruby
au BufNewFile,BufRead *.rk set filetype=ruby
au BufNewFile,BufRead *.mk set filetype=make
au BufNewFile,BufRead *.cpp set filetype=cpp
au BufNewFile,BufRead *.h set filetype=cpp
au BufNewFile,BufRead qsub* set filetype=sh

"------------------------------------------------------------
" spacemacs bindings
"------------------------------------------------------------

let mapleader = "\<Space>"

" split windows
nnoremap <leader>wv <C-W><C-V>
nnoremap <leader>ws <C-W>s
nnoremap <leader>wc <C-W>q
nnoremap <leader>wh <C-W>h
nnoremap <leader>wj <C-W>j
nnoremap <leader>wk <C-W>k
nnoremap <leader>wl <C-W>l

" save file
nnoremap <leader>fs :w<CR>

" toggles
nnoremap <leader>tw :set list!<CR>

"------------------------------------------------------------
" taglist settings
"------------------------------------------------------------

let Tlist_Process_File_Always = 1
let Tlist_Show_One_File = 1
nnoremap <leader>t :TlistToggle<CR>

"------------------------------------------------------------
" NERDTree settings
"------------------------------------------------------------

" disable default <C-j> keybinding (interferes with other mappings)
let NERDTreeMapJumpNextSibling=''
" disable default <C-k> keybinding (interferes with other mappings)
let NERDTreeMapJumpPrevSibling=''

"------------------------------------------------------------
" VimDiff settings
"------------------------------------------------------------

au FilterWritePre * if &diff | colorscheme elmindreda | endif

"------------------------------------------------------------
" custom text objects
"------------------------------------------------------------

call textobj#user#plugin('path', {
\ 'path': {
\ 'pattern': '\f', 'select': ['ap', 'ip']
\ },
\ 'pathcomponent': {
\ 'pattern': "[^\/\\s\"']\\+", 'select': ['af', 'if']
\ }
\ })

"------------------------------------------------------------
" editing natural language text
"------------------------------------------------------------

" turn on "word wrap"
nnoremap <leader>W :set wrap\|set linebreak\|set nolist<CR>

"------------------------------------------------------------
" IndentWise plugin
"------------------------------------------------------------

map <C-b> <Plug>(IndentWisePreviousEqualIndent)
map <C-f> <Plug>(IndentWiseNextEqualIndent)

"------------------------------------------------------------
" vim-snipmate plugin (requires vim 7.4)
"------------------------------------------------------------

" clear selected text in SELECT mode (used for tab stops
" within snippets)
snoremap <C-d> <space><C-h>

"------------------------------------------------------------
" unorganized settings
"------------------------------------------------------------

set tabstop=4
set shiftwidth=4
set noexpandtab
set ignorecase
set smartcase
set incsearch
set relativenumber

"colorscheme evening
"colorscheme blue
colorscheme default

nmap <C-J> <C-W>w
nmap <C-K> <C-W>W
nmap <C-H> <C-W>+
nmap <C-L> <C-W>-
nmap <S-H> 2<C-W><
nmap <S-L> 2<C-W>>
imap <C-g> <Esc>

set isfname=@,48-57,/,.,-,_,+,,,#,$,%,~
" backspace over everything in insert mode
set backspace=indent,eol,start
set pastetoggle=<F2>
syntax on
