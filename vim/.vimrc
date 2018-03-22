" https://github.com/atejeda/dotfiles

" plugins:
"   https://github.com/tpope/vim-fugitive
"   https://github.com/kien/ctrlp.vim
"   https://github.com/scrooloose/nerdtree
"   https://github.com/bling/vim-airline
"   https://github.com/scrooloose/nerdcommenter
"   https://github.com/Lokaltog/vim-easymotion
"   https://github.com/davidhalter/jedi-vim
"   https://github.com/scrooloose/syntastic
"   https://github.com/editorconfig/editorconfig-vim
"
execute pathogen#infect()

syntax on
colorscheme atom-dark-256 "default

filetype plugin indent on

autocmd FileType make setlocal noexpandtab
autocmd GUIEnter * set visualbell t_vb=

set tw=80
set cc=+1
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





