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
Plugin 'easymotion/vim-easymotion'
Plugin 'scrooloose/nerdtree'
Plugin 'vimscripts/taglist.vim'
Plugin 'majutsushi/tagbar'
Plugin 'kien/ctrlp.vim'
call vundle#end()

filetype plugin indent on

filetype on

"------------------------------------------------------------
" python settings
"------------------------------------------------------------

au BufNewFile,BufRead *.py set filetype=python

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
" unorganized settings
"------------------------------------------------------------

set tabstop=4
set shiftwidth=4
set noexpandtab
set ignorecase
set smartcase
set incsearch
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
filetype plugin indent on
syntax on
