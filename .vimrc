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
set signcolumn=yes
" set inccommand=nosplit
set hidden

set equalalways

filetype plugin indent on

autocmd VimResized * wincmd =
autocmd BufRead,BufNewFile *.fish set filetype=sh

let mapleader = "\<Space>"

call plug#begin('~/.vim/plugged')

Plug 'https://github.com/tpope/vim-fugitive.git'
Plug 'https://github.com/morhetz/gruvbox.git'
Plug 'https://github.com/tpope/vim-surround.git'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'https://github.com/vim-nerdtree/nerdtree.git'
Plug 'https://github.com/airblade/vim-gitgutter.git'
Plug 'https://github.com/vim-scripts/json-formatter.vim.git'
Plug 'https://github.com/joshdick/onedark.vim.git'
Plug 'https://github.com/Chiel92/vim-autoformat.git'
Plug 'https://github.com/vim-scripts/taglist.vim.git'
Plug 'christoomey/vim-conflicted'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'https://github.com/easymotion/vim-easymotion.git'
Plug 'https://github.com/frazrepo/vim-rainbow.git'
Plug 'OmniSharp/omnisharp-vim'
Plug 'dense-analysis/ale' ", { 'on':  'ALEToggle' }
Plug 'pprovost/vim-ps1'
Plug 'vim-airline/vim-airline'
Plug 'mhartington/oceanic-next'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'https://github.com/pseewald/vim-anyfold.git'
Plug 'andymass/vim-matchup'
Plug 'jiangmiao/auto-pairs'
Plug 'https://github.com/tpope/vim-unimpaired.git'

call plug#end()

set laststatus=2

" Tlist configs
let Tlist_Use_Horiz_Window = 0  
let Tlist_WinWidth = 70

" Config settings
set updatetime=200

let g:easytags_async = 1

let g:NERDTreeWinSize=60

" Colorscheme
if (has("termguicolors"))
 set termguicolors
endif

colorscheme gruvbox

autocmd VimEnter * command! -bang -nargs=? GFiles call fzf#vim#gitfiles(<q-args>, {'options': '--no-preview'}, <bang>0)
autocmd VimEnter * command! -bang -nargs=? Files call fzf#vim#files(<q-args>, {'options': '--no-preview'}, <bang>0)
autocmd VimEnter * command! -bang -nargs=? Buffers call fzf#vim#buffers(<q-args>, {'options': '--no-preview'}, <bang>0)

" Custom hotkeys
nnoremap <Leader>fd :Files<CR>
nnoremap <Leader>fl :GFiles<CR>
noremap <leader>cm :Commands<CR>
noremap <Leader>uh :GitGutterUndoHunk<CR>
noremap <Leader>nt :NERDTreeFind<CR>
noremap <Leader>fb :Buffers<CR>
noremap <Leader>rg :CocSearch 
noremap <Leader>tl :TlistOpen<CR>
noremap <Leader>hs :History:<CR>
noremap <Leader>h/ :History/<CR>
noremap <Leader>bd :bd!<CR>
noremap <Leader>wh :MatchupWhereAmI<CR>
noremap <Leader>af :AnyFoldActivate<CR>

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

" Fugitive hotkeys
nmap <leader>gs :G<CR>
nmap <leader>gj :diffget //3<CR>
nmap <leader>gf :diffget //2<CR>

" CocHotkeys
" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" Comment/uncomment these for now
" GoTo code navigation.
nmap <leader>gd <Plug>(coc-definition)
nmap <leader> gy <Plug>(coc-type-definition)
nmap <leader> gi <Plug>(coc-implementation)
nmap <leader> gr <Plug>(coc-references)

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" Use K to show documentation in preview window.
nmap <leader>dc :call <SID>show_documentation()<CR>
