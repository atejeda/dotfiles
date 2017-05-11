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
"   https://github.com/editorconfig/editorconfig-vim
"
execute pathogen#infect()

"if $HOSTNAME == "hangar18"
if has ('gui_running')
    set guifont=Monospace\ 8
    set guifont=Hack\ 8
endif

syntax on
colorscheme atom-dark-256

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
"set fdc=4
"set fdl=1
set number
set splitright
set splitbelow

:set mouse=n
" (I use 'n', but 'a' also works)
" " and you have to set the tty mouse type
:set ttymouse=xterm2

" vim hints
" http://vim.wikia.com/wiki/Mapping_keys_in_Vim_-_Tutorial_(Part_1)
":map #list of maps
":verbose map , # print the map on that key
"Ctrl-I #Tab
"Ctrl-[ #Esc
"Ctrl-M #Enter
"Ctrl-H #backspace
"Ctrl-R #undo
"
"Ctrl+W +/-: increase/decrease height
"Ctrl+W >/<: increase/decrease width
"Ctrl+W _: set height (ex. 50<C-w>_)
"Ctrl+W |: set width (ex. 50<C-w>|)
"Ctrl+W =: equalize width and height of all windows
"See also: :help CTRL_W
"
" :resize [+-]N - resize a horizontal split, increasing or decreasing
" height by N characters.
" :vertical resize [+-]N - resize a vertical split, increasing or
" decreasing height by N characters.
" :resize N - resize a horizontal split, setting height to N
" characters.
" vertical resize N - resize a vertical split, setting width
" to N characters.
" 
" :set mouse=n
" (I use 'n', but 'a' also works)
" and you have to set the tty mouse type
" :set ttymouse=xterm2

" key maps
:noremap <C-S-B> :make<CR>
:noremap <C-S-B> <C-C>:make<CR>
:inoremap <C-S-B> <C-O>:make<CR>

:nmap llb :ls<CR>:buffer<Space>

:noremap <C-S> :update<CR>
:vnoremap <C-S> <C-C>:update<CR>
:inoremap <C-S> <C-O>:update<CR>
