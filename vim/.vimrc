" vundle setup
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'kien/ctrlp.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'vim-airline/vim-airline'
Plugin 'scrooloose/syntastic'
Plugin 'flazz/vim-colorschemes'
Plugin 'felixhummel/setcolors.vim'
Plugin 'Valloric/YouCompleteMe'
Plugin 'dart-lang/dart-vim-plugin'
Plugin 'thosakwe/vim-flutter'
Plugin 'neoclide/coc.nvim'
call vundle#end()

filetype plugin indent on

syntax on
colorscheme nord "default / with iterm nord

filetype plugin indent on

autocmd FileType make setlocal noexpandtab
autocmd GUIEnter * set visualbell t_vb=

"set tw=80
"set cc=+1
set encoding=utf-8
set fileencoding=utf-8
set t_Co=256
set tabstop=4
set shiftwidth=4
set expandtab
set showmatch "set show matching parenthesis
set ignorecase "ignore case when searching
set smartcase "ignore case if search pattern is all lowercase, case-sensitive otherwise
set hlsearch "highlight search terms
set incsearch "show search matches as you type
set history=1000 "remember more commands and search history
set undolevels=1000 "use many muchos levels of undo
set wildignore=*.swp,*.bak,*.pyc,*.class
set title "change the terminal's title
set visualbell "don't beep
set noerrorbells "don't beep
set nobackup
set noswapfile
set noerrorbells visualbell t_vb=
set laststatus=2
"set fdc=4
"set fdl=1
set number
set splitright
set splitbelow

" relartive numbers
:set number relativenumber
:augroup numbertoggle
:  autocmd!
:  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
:  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
:augroup END
