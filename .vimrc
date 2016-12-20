" Allow `jk` to return to Normal mode
:imap jk <Esc>

" Enable filetype detection
filetype on

" Enable syntax highlighting
syntax enable
set background=dark
colorscheme solarized

" Don't try to be vi compatible
set nocompatible
" Show line numbers
set number
" Blink cursor on error instead of beeping (grr)
set visualbell
" fast terminal for smoother redrawing
set ttyfast

" Tabs render as 4 spaces
set tabstop=4
" Tabs converted to whitespace
set expandtab
" Tab key adds 4 spaces
set softtabstop=4
" Width for auto-indents
set shiftwidth=4
