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

set wildignore+=**/bin/**,**/obj/**,**/*.dll

let mapleader = "\<Space>"

nnoremap J <C-e>
nnoremap K <C-y>

noremap <leader>cm :Commands<CR>
noremap y "+y
noremap p "+p
noremap P "+P
vnoremap d "_d
nnoremap d "_d
" vnoremap c "_c 
nnoremap c "_c

nmap $ g_
vmap $ g_

call plug#begin('~/.vim/plugged')

Plug 'https://github.com/tpope/vim-fugitive.git'
Plug 'https://github.com/tpope/vim-surround.git'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'https://github.com/vim-nerdtree/nerdtree.git'
Plug 'https://github.com/itchyny/lightline.vim.git'
Plug 'https://github.com/airblade/vim-gitgutter.git'
Plug 'https://github.com/vim-scripts/json-formatter.vim.git'
Plug 'https://github.com/joshdick/onedark.vim.git'
Plug 'https://github.com/Chiel92/vim-autoformat.git'
Plug 'https://github.com/pseewald/vim-anyfold.git'
Plug 'https://github.com/SirVer/ultisnips.git'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'https://github.com/vim-scripts/taglist.vim.git'
Plug 'lervag/vimtex'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'christoomey/vim-conflicted'

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
let Tlist_Use_Horiz_Window = 1  
let Tlist_WinHeight = 15

" Autosave configs 
let g:auto_save = 1
let g:auto_save_no_updatetime = 1
let g:auto_save_in_insert_mode = 0

" No ycm for cs files 
let g:ycm_filetype_blacklist = {'cs': 1}

set updatetime=200

let g:easytags_async = 1

let g:OmniSharp_server_stdio = 1

let g:ale_linters = {
\ 'cs': ['OmniSharp']
\}

" More omnisharp stuff
let g:ale_echo_cursor = 0
let g:OmniSharp_highlight_types = 2
let g:OmniSharp_selector_ui = 'fzf'

let g:NERDTreeWinSize=60

" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<c-g>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

colorscheme onedark
hi! Normal guibg=NONE ctermbg=NONE

" Omnisharp configs
augroup omnisharp_commands
    autocmd!
    " The following commands are contextual, based on the cursor position.
    autocmd FileType cs nnoremap <buffer> gd :OmniSharpGotoDefinition<CR>
    autocmd FileType cs nnoremap <buffer> <Leader>fi :OmniSharpFindImplementations<CR>
    autocmd FileType cs nnoremap <buffer> <Leader>fs :OmniSharpFindSymbol<CR>
    autocmd FileType cs nnoremap <buffer> <Leader>fu :OmniSharpFindUsages<CR>

    " Finds members in the current buffer
    autocmd FileType cs nnoremap <buffer> <Leader>fm :OmniSharpFindMembers<CR>

    autocmd FileType cs nnoremap <buffer> <Leader>fx :OmniSharpFixUsings<CR>
    autocmd FileType cs nnoremap <buffer> <Leader>tt :OmniSharpTypeLookup<CR>
    autocmd FileType cs nnoremap <buffer> <Leader>dc :OmniSharpDocumentation<CR>
    autocmd FileType cs nnoremap <buffer> <C-\> :OmniSharpSignatureHelp<CR>
    autocmd FileType cs inoremap <buffer> <C-\> <C-o>:OmniSharpSignatureHelp<CR>

    " Navigate up and down by method/property/field
    autocmd FileType cs nnoremap <buffer> <C-k> :OmniSharpNavigateUp<CR>
    autocmd FileType cs nnoremap <buffer> <C-j> :OmniSharpNavigateDown<CR>

    " Find all code errors/warnings for the current solution and populate the quickfix window
    autocmd FileType cs nnoremap <buffer> <Leader>cc :OmniSharpGlobalCodeCheck<CR>
augroup END

" Contextual code actions (uses fzf, CtrlP or unite.vim when available)
nnoremap <Leader><Space> :OmniSharpGetCodeActions<CR>
" Run code actions with text selected in visual mode to extract method
xnoremap <Leader><Space> :call OmniSharp#GetCodeActions('visual')<CR>

" Rename with dialog
nnoremap <Leader>nm :OmniSharpRename<CR>
nnoremap <F2> :OmniSharpRename<CR>
" Rename without dialog - with cursor on the symbol to rename: `:Rename newname`
command! -nargs=1 Rename :call OmniSharp#RenameTo("<args>")

nnoremap <Leader>cf :OmniSharpCodeFormat<CR>

" Start the omnisharp server for the current solution
" General Hotkeys
nnoremap <Leader>ss :OmniSharpStartServer<CR>
nnoremap <Leader>sp :OmniSharpStopServer<CR>
nnoremap <Leader>sr :OmniSharpRestartServer<CR>
nnoremap <Leader>ad :ALEDetail<CR>
nnoremap <Leader><Leader>fd :Files!<CR>
nnoremap <Leader>fd :GFiles!<CR>
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

" Command for exectuing diffs
function EmptyDiff(fileType)
	execute 'tabe difftwo.' . a:fileType
	execute 'vs diffone.' . a:fileType
	windo difft
endfunction

if !has('gui_running')
  set t_Co=256
endif

" Set html syntax highlighting to cshtml
au BufRead,BufNewFile *.cshtml set filetype=html

" Fzf configs
imap <c-x><c-x> <plug>(fzf-complete-path)
