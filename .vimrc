" 
" atejeda
"
" plugins:
"   https://github.com/tpope/vim-fugitive
"   https://github.com/kien/ctrlp.vim
"   https://github.com/scrooloose/nerdtree
"   https://github.com/bling/vim-airline
"   https://github.com/scrooloose/nerdcommenter
"   https://github.com/Lokaltog/vim-easymotion
"   https://github.com/davidhalter/jedi-vim
"   https://github.com/scrooloose/syntastic
"

" Colors setup
" Update your .screenrc file and add the following:
" attrcolor b ".I"
" termcapinfo xterm 'Co#256:AB=\E[48;5;%dm:AF=\E38;5;%dm'
" defbce "on"
set t_Co=256

" pathogen and plugins
execute pathogen#infect()

" gui configuration
if has('gui_running')
    "set guifont=Monospace\ 7
    colors pablo
endif

"
" configuration
"

syntax on
" tab = 4 spaces
filetype plugin indent on
set tabstop=4
set shiftwidth=4
set expandtab
" set softtabstop=4 " makes the spaces feel like real tabs
autocmd FileType make setlocal noexpandtab
"autocmd FileType * set tabstop=2|set shiftwidth=2|set noexpandtab " python
"autocmd FileType python set tabstop=4|set shiftwidth=4|set expandtab " python
" stuff
"set nowrap " don't wrap lines
"set number " always show line numbers
set showmatch " set show matching parenthesis
set ignorecase " ignore case when searching
set smartcase " ignore case if search pattern is all lowercase, case-sensitive otherwise
set hlsearch " highlight search terms
set incsearch " show search matches as you type
set history=1000 " remember more commands and search history
set undolevels=1000 " use many muchos levels of undo
set wildignore=*.swp,*.bak,*.pyc,*.class
set title " change the terminal's title
set visualbell " don't beep
set noerrorbells " don't beep
set nobackup
set noswapfile
" disable visual bell
set noerrorbells visualbell t_vb=
autocmd GUIEnter * set visualbell t_vb=

" airspeed
set laststatus=2
