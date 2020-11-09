set number
set relativenumber
set ignorecase
set incsearch
set backspace=2
set nohlsearch
set nowrap
set wildmode=longest,list,full

syntax on

set expandtab
set smarttab
set textwidth=120
set ts=4 sw=4
set autoindent
set smartindent

filetype plugin indent on

autocmd BufRead,BufNewFile *.fish set filetype=sh

let mapleader = "\<Space>"

call plug#begin('~/.vim/plugged')

" Plug 'https://github.com/tpope/vim-fugitive.git'
Plug 'https://github.com/tpope/vim-surround.git'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'https://github.com/vim-nerdtree/nerdtree.git'
Plug 'https://github.com/airblade/vim-gitgutter.git'
Plug 'https://github.com/vim-scripts/json-formatter.vim.git'
Plug 'https://github.com/joshdick/onedark.vim.git'
Plug 'https://github.com/Chiel92/vim-autoformat.git'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'https://github.com/vim-scripts/taglist.vim.git'
Plug 'christoomey/vim-conflicted'
Plug 'neoclide/coc.nvim', {'branch': 'master', 'do': 'yarn install --frozen-lockfile'}
" Plug 'dense-analysis/ale'
" Plug 'https://github.com/ycm-core/YouCompleteMe.git'

call plug#end()

set laststatus=2

" Tlist configs
let Tlist_Use_Horiz_Window = 0  
let Tlist_WinWidth = 70

set updatetime=200

let g:easytags_async = 1

let g:NERDTreeWinSize=60

colorscheme onedark

" General hotkeys
nnoremap <Leader>ad :ALEDetail<CR>
nnoremap <Leader><Leader>fd :GFiles!<CR>
nnoremap <Leader>fd :GFiles!<CR>
nnoremap <Leader>cd :Files!<CR>
noremap <leader>cm :Commands<CR>
noremap <Leader>uh :GitGutterUndoHunk<CR>
noremap <Leader>nt :NERDTreeFind<CR>
noremap <Leader>fb :Buffers!<CR>
noremap <Leader>an :ALENext<CR>
noremap <Leader>rg :Rg! 
noremap <Leader>ed :call EmptyDiff("
noremap <Leader>tl :TlistOpen<CR>
noremap <Leader>hs :History:<CR>

" Copy paste hotkeys
" Paste
noremap <Leader>p "+p
vnoremap <Leader>p "+p

noremap <Leader>y "+y
vnoremap <Leader>y "+y

noremap <Leader>d "_d
vnoremap <Leader>d "_d

nmap $ g_
vmap $ g_
