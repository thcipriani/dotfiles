" Basic options ----------------------------------------------------------- {{{
let mapleader=","
set encoding=utf-8
set modelines=0
set autoindent
set showmode
set showcmd
set hidden
set visualbell
set cursorline
set ruler
set laststatus=2
set history=1000
set cpoptions+=J
set title
set guioptions=m
set number
set nowrap
call pathogen#infect()
call pathogen#helptags()
call pathogen#runtime_append_all_bundles()
syntax on
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set background=light
colorscheme Tomorrow-Night-Eighties
set ignorecase
set smartcase
set modelines=0
set undofile

"make regex sane
nnoremap / /\v
vnoremap / /\v

"search highlights
set incsearch
set showmatch
set hlsearch

"no backup files
set nobackup

"only in case you don't want a backup file while editing
set nowritebackup

"no swap files
set noswapfile

filetype plugin indent on
set ofu=syntaxcomplete#Complete
set guifont=inconsolata\ 16

" Included for PowerLine
set nocompatible " Disable vi-compatibility
set t_Co=256 " Explicitly tell vim that the terminal has 256 colors
let g:Powerline_symbols = 'fancy'

" Status line ------------------------------------------------------------- {{{

augroup ft_statuslinecolor
    au!

    au InsertEnter * hi StatusLine ctermfg=196 guifg=#FF3145
    au InsertLeave * hi StatusLine ctermfg=130 guifg=#CD5907
augroup END

set statusline=%f    " Path.
set statusline+=%m   " Modified flag.
set statusline+=%r   " Readonly flag.
set statusline+=%w   " Preview window flag.

set statusline+=\    " Space.

set statusline+=%=   " Right align.

" Line and column position and counts.
set statusline+=\ (line\ %l\/%L,\ col\ %03c)

" }}}

" Convenient Mappings ------------------------------------------------------------- {{{
"
" Dumb escape
imap JJ <ESC>
vmap JJ <ESC>

" NERDTree Settings
map <F2> :NERDTreeToggle<CR>
nnoremap <leader><space> :noh<cr>

" Shortcut to rapidly toggle `set list`
nmap <leader>l :set list!<CR>

" Use the same symbols as TextMate for tabstops and EOLs
set listchars=tab:▸\ ,eol:¬

"tab for bracket pairs
nnoremap <tab> %
vnoremap <tab> %
"
" Toggle Linenumbers in normal mode
map <F3> :NumbersToggle<CR>

"solarized dark
nmap <unique> <F5> <Plug>ToggleBackground
imap <unique> <F5> <Plug>ToggleBackground
vmap <unique> <F5> <Plug>ToggleBackground

"Opens a vertical split and switches over (,v)  
nnoremap <leader>v <C-w>v<C-w>l  

"Moves around split windows
nnoremap <leader>w <C-w><C-w>  

"Delete Blanklines
nnoremap <leader>S :v/\S/d<CR>

"Double Space
nnoremap <leader>D :g/^/put_<CR>      

"Real Returns
nnoremap <leader>R :%s/\r/\r/g<CR>
" }}}
