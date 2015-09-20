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
Plugin 'vim-scripts/InsertChar'
Plugin 'jeetsukumaran/vim-indentwise'
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
nnoremap <leader>w :set wrap\|set linebreak\|set nolist<CR>

"------------------------------------------------------------
" InsertChar plugin
"------------------------------------------------------------

nnoremap <Space> :<C-U>call InsertChar#insert(v:count1)<CR>

"------------------------------------------------------------
" IndentWise plugin
"------------------------------------------------------------

map <C-b> <Plug>(IndentWisePreviousEqualIndent)
map <C-f> <Plug>(IndentWiseNextEqualIndent)

"------------------------------------------------------------
" insert mode keybindings
"------------------------------------------------------------

inoremap <C-c> <nop>
inoremap <CR> <Esc>l

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
syntax on
