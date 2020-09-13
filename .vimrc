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

autocmd Filetype * AnyFoldActivate

autocmd BufRead,BufNewFile *.fish set filetype=sh

let mapleader = "\<Space>"

nnoremap J <C-e>
nnoremap K <C-y>

noremap <leader>cm :Commands<CR>

nmap $ g_
vmap $ g_

call plug#begin('~/.vim/plugged')

Plug 'https://github.com/tpope/vim-fugitive.git'
Plug 'https://github.com/tpope/vim-surround.git'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'https://github.com/vim-nerdtree/nerdtree.git'
Plug 'https://github.com/airblade/vim-gitgutter.git'
Plug 'https://github.com/vim-scripts/json-formatter.vim.git'
Plug 'https://github.com/joshdick/onedark.vim.git'
Plug 'https://github.com/Chiel92/vim-autoformat.git'
Plug 'https://github.com/pseewald/vim-anyfold.git'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'https://github.com/vim-scripts/taglist.vim.git'
Plug 'lervag/vimtex'
Plug 'christoomey/vim-conflicted'
Plug 'dense-analysis/ale'
Plug 'https://github.com/ycm-core/YouCompleteMe.git'

call plug#end()

set laststatus=2

" Latex configurations
if !exists('g:ycm_semantic_triggers')
    let g:ycm_semantic_triggers = {}
  endif
  au VimEnter * let g:ycm_semantic_triggers.tex=g:vimtex#re#youcompleteme
set foldlevel=99 " Open all folds

let g:tex_flavor='latex'
let g:vimtex_view_method='zathura'
let g:vimtex_quickfix_mode=0
" set conceallevel=1
let g:tex_conceal='abdmg'

" Tlist configs
let Tlist_Use_Horiz_Window = 0  
let Tlist_WinWidth = 70

set updatetime=200

let g:easytags_async = 1

let g:NERDTreeWinSize=60

colorscheme onedark

" General Hotkeys
nnoremap <Leader>ad :ALEDetail<CR>
nnoremap <Leader><Leader>fd :GFiles!<CR>
nnoremap <Leader>fd :Files!<CR>
noremap <Leader>uh :GitGutterUndoHunk<CR>
noremap <Leader>nt :NERDTreeFind<CR>
noremap <Leader>fb :Buffers!<CR>
noremap <Leader>an :ALENext<CR>
noremap <Leader>rg :Rg! 
noremap <Leader>ed :call EmptyDiff("
noremap <Leader>tl :TlistOpen<CR>

" Git mappings 
nmap <Leader>gs :Gstatus<CR>
nmap <Leader>gl :Glog<CR>
nmap <Leader>gd :Gvdiffsplit<CR>
