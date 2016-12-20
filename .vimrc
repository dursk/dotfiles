" Allow `jk` to return to Normal mode
:imap jk <Esc>
" Move between split windows 
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

set autoindent

filetype on

syntax enable
set background=dark
colorscheme solarized

" Use everything from python.vim
let python_highlight_all = 1

" Don't try to be vi compatible
set nocompatible
" Show line numbers
set number
" Blink cursor on error instead of beeping (grr)
set visualbell
" fast terminal for smoother redrawing
set ttyfast

" Search as you are typing
set incsearch
" Highlight search results
set hlsearch
" ,<space> undos search highlighting
nnoremap <leader><space> :nohlsearch<CR>

" Tabs render as 4 spaces
set tabstop=4
" Tabs converted to whitespace
set expandtab
" Tab key adds 4 spaces
set softtabstop=4
" Width for auto-indents
set shiftwidth=4
