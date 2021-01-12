set number
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
" set inccommand=nosplit
set hidden

filetype plugin indent on

autocmd BufRead,BufNewFile *.fish set filetype=sh

let mapleader = "\<Space>"

call plug#begin('~/.vim/plugged')

Plug 'https://github.com/morhetz/gruvbox.git'
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
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'https://github.com/easymotion/vim-easymotion.git'
Plug 'https://github.com/frazrepo/vim-rainbow.git'
Plug 'OmniSharp/omnisharp-vim'
Plug 'dense-analysis/ale'

call plug#end()

set laststatus=2

" Tlist configs
let Tlist_Use_Horiz_Window = 0  
let Tlist_WinWidth = 70

set updatetime=200

let g:easytags_async = 1

let g:NERDTreeWinSize=60

colorscheme gruvbox

nnoremap <Leader>fd :Files<CR>
noremap <leader>cm :Commands<CR>
noremap <Leader>uh :GitGutterUndoHunk<CR>
noremap <Leader>nt :NERDTreeFind<CR>
noremap <Leader>fb :Buffers<CR>
noremap <Leader>rg :CocSearch 
noremap <Leader>tl :TlistOpen<CR>
noremap <Leader>hs :History:<CR>
noremap <Leader>bd :bd!<CR>

nnoremap <BS> <C-^>

" Copy paste hotkeys
" Paste
noremap p "+p
vnoremap p "+p
noremap P "+P
vnoremap P "+P

noremap y "+y
vnoremap y "+y

noremap <Leader>d "_d
vnoremap <Leader>d "_d

nmap $ g_
vmap $ g_

nmap s <Plug>(easymotion-overwin-f2)

" Turn on case-insensitive feature
let g:EasyMotion_smartcase = 1

" JK motions: Line motions
map J <Plug>(easymotion-j)
map K <Plug>(easymotion-k)

" Exec shell scripts
command Exec set splitright | vnew | set filetype=json | read !sh # 


