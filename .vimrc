" atejeda

" checkout the modules by:
"   git submodule init
"   git sobmodule update

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
execute pathogen#infect()

if has('gui_running')
"    colors Sunburst
"set guifont=Monospace\ 9
"if $HOSTNAME == "hangar18"
"set guifont=Monospace\ 9
"else
"set guifont=Hack\ 9
"endif
"colors Sunburst
"else
"colors railscasts
endif

syntax on

filetype plugin indent on

autocmd FileType make setlocal noexpandtab
autocmd GUIEnter * set visualbell t_vb=

set encoding=utf-8
set fileencoding=utf-8
set t_Co=256
set tabstop=4
set shiftwidth=4
set expandtab
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
set noerrorbells visualbell t_vb=
set laststatus=2

" vim hints
" http://vim.wikia.com/wiki/Mapping_keys_in_Vim_-_Tutorial_(Part_1)
":map #list of maps
":verbose map , # print the map on that key
"Ctrl-I #Tab
"Ctrl-[ #Esc
"Ctrl-M #Enter
"Ctrl-H #backspace

" key maps
" invoke a local makefile
:nmap <C-S-B> :make<CR>
" list and swith to buffer
:nmap llb     :ls<CR>:buffer<Space> 
