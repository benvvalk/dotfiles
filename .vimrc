"------------------------------------------------------------
" Vundle setup begin
"------------------------------------------------------------

set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" plugins list
Plugin 'tpope/vim-fugitive'
"Plugin 'bling/vim-airline'
Plugin 'easymotion/vim-easymotion'
Plugin 'scrooloose/nerdtree'
Plugin 'vimscripts/taglist.vim'
Plugin 'majutsushi/tagbar'
"
" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just
":PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to
"auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

"------------------------------------------------------------
" Vundle setup end
"------------------------------------------------------------

"execute pathogen#infect()

"------------------------------------------------------------
" airline conf (fancy status bar)
"------------------------------------------------------------
"
"let g:airline_powerline_fonts = 1
"set laststatus=2

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
