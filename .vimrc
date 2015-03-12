" atejeda

syntax on
"colors slate
	
" font for gvim
"if has('gui_running')
"    set guifont=Monospace\ 7
"endif

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
set nowrap " don't wrap lines
set number " always show line numbers
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
