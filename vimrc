" Vim. Live it. ------------------------------------------------------- {{{
noremap <up> <nop>
noremap <down> <nop>
noremap <left> <nop>
noremap <right> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>
inoremap <up> <nop>
" }}}

" What is this 'Vee-Eye' of which you speak? -------------------------- {{{
set nocompatible
" }}}

" Pathogen ------------------------------------------------------------ {{{
call pathogen#infect()
call pathogen#helptags()
call pathogen#runtime_append_all_bundles()
" }}}

" Lifesaver ----------------------------------------------------------- {{{
if &term =~ '256color'
  " Disable Background Color Erase (BCE) so that color schemes
  " work properly when Vim is used inside tmux and GNU screen.
  " See also http://snk.tuxfamily.org/log/vim-256color-bce.html
  set t_ut=
endif
" }}}

" Basic options ------------------------------------------------------- {{{
let mapleader=','
set t_Co=256              " My terminal's got all those colors, yo

set title                 " Change the terminal title
set encoding=utf-8        " Show utf-8 chars
set showcmd               " count highlighted
set ruler                 " Show where I am in the command area
set showmode              " -- INSERT (appreciation)-- :)
set laststatus=2          " always show the status line 
                          " ↪ (0 = never, 1 = default [multi-window only])

set modelines=0           " Don't read first/last lines of file for settings
set hidden                " Stash unwritten files in buffer
set vb                    " Don't beep at me
set cursorline            " Highlight current line
set scrolloff=3           " Start scrolling when I'm 3 lines from top/bottom
set history=1000          " Remember commands and search history
set backspace=2           " Backspace over indent, eol, and insert
set mousehide             " Hide the mouse pointer while typing

set binary                " Don’t add empty newlines at the end of files
set noeol

set number                " Show linenumbers
set nowrap                " Turn off linewrap
set list                  " Show invisible chars
set tabstop=4             " 4 spaces
set shiftwidth=4          " 4 spaces
set softtabstop=4         " 4 spaces
set expandtab             " Expand tabs to spaces

set hlsearch              " highlight my search
set incsearch             " incremental search
set wrapscan              " Set the search scan to wrap around the file

set ignorecase            " when searching
set smartcase             " …unless I use an uppercase character

syntax on                 " Syntax highlighting
syntax sync minlines=256  " Makes big files slow
set synmaxcol=2048        " Also long lines are slow
set autoindent            " try your darndest to keep my indentation
set smartindent           " Be smarter about indenting dummy
set formatoptions=tcqr    " I like smart comments

"no backup or swap files
set nobackup
set nowritebackup
set noswapfile

" Colorscheme
" https://github.com/altercation/vim-colors-solarized
" colorscheme solarized
" set background=dark

colorscheme molokai

" GUI Font (same as my gnome-terminal font)
" https://github.com/adobe/source-code-pro
set guifont=Source\ Code\ Pro\ 14

" Use the same symbols as TextMate for tabstops and EOLs
set listchars=tab:▸\ ,eol:¬

" Version 7.3 (703) --------------------------------------------------- {{{
if v:version >= 703
  set colorcolumn=75
  hi ColorColumn ctermbg=234
  set undodir=~/.vim-undo
  set undofile
  set undolevels=1000 "max number of changes that can be undone
  set undoreload=10000 "max number lines to save for undo on buffer reload

  " Toggle line numbers in normal mode, set by default
  set relativenumber
  function! NumberToggle()
    if(&relativenumber == 1)
      set number
    else
      set relativenumber
    endif
  endfunc

  nnoremap <leader>n :call NumberToggle()<cr>
endif
" }}}

" Dumb osx -------- --------------------------------------------------- {{{
if has("unix")
    let s:uname = system("uname")
    if s:uname == "Darwin\n"
        set clipboard=unnamed
    endif
endif
" }}}

" Convenient Mappings ------------------------------------------------- {{{
" Make regex sane
noremap / /\v

" Dumb escape
inoremap JJ <ESC>
vnoremap JJ <ESC>

" un-highlight search results
noremap <leader><space> :noh<cr>

" Toggle auto-indent before clipboard paste
set pastetoggle=<leader>p

" Shortcut to rapidly toggle `set list`
nnoremap <leader>l :set list!<CR>

" Normal/Visual tab for bracket pairs
nnoremap <tab> %
vnoremap <tab> %

" Insermode tab for code completion
inoremap <tab> <C-n>
inoremap <S-tab> <C-p>

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

"Autocomplete on tab https://github.com/garybernhardt/dotfiles/blob/master/.vimrc
function! InsertTabWrapper()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<tab>"
    else
        return "\<c-n>"
    endif
endfunction
inoremap <tab> <c-r>=InsertTabWrapper()<cr>
inoremap <s-tab> <c-p>

" }}}

" Status line --------------------------------------------------------- {{{
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

" Development Tools --------------------------------------------------- {{{
" Tagbar (requires Exuberant ctags 5.5+)
noremap <leader>c :TagbarToggle<CR>

" Xdebug local debugger
let g:vdebug_options = {'server': '33.33.33.1', 'port': '9000', 'path_maps' : {'/srv/www/local.sa2.dev': '/Users/tyler/Development/upsync-vagrant/shared/sa2'} }
" }}}

" NERDTree Settings---------------------------------------------------- {{{
"map <leader>t :NERDTreeToggle<CR>
noremap <leader>t :NERDTreeTabsToggle<CR>
" }}}

" Included for PowerLine ---------------------------------------------- {{{
let g:Powerline_symbols = 'fancy'
" }}}

" CtrlP --------------------------------------------------------------- {{{
let g:ctrlp_max_files = 0 " Set no max file limit
let g:ctrlp_working_path_mode = 0 " Search current directory not project root
"}}}