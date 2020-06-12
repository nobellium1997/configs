set number
set relativenumber
set ignorecase
set incsearch
set backspace=2
set nohlsearch
set nowrap
set wildmode=longest,list,full
syntax on

filetype plugin indent on

autocmd Filetype * AnyFoldActivate

set wildignore+=**/bin/**,**/obj/**,**/*.dll

let mapleader = "\<Space>"

set ts=4 sw=4

nnoremap J <C-e>
nnoremap K <C-y>

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

Plug 'https://github.com/vim-scripts/vim-auto-save.git'

Plug 'https://github.com/xolox/vim-easytags.git'

Plug 'https://github.com/xolox/vim-misc.git'

Plug 'https://github.com/tpope/vim-fugitive.git'

Plug 'https://github.com/dense-analysis/ale.git'

Plug 'https://github.com/tpope/vim-surround.git'

Plug 'https://github.com/KangOl/vim-pudb.git'

Plug 'https://github.com/OmniSharp/omnisharp-vim.git'

Plug 'prabirshrestha/asyncomplete.vim'

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }

Plug 'junegunn/fzf.vim'

Plug 'https://github.com/vim-nerdtree/nerdtree.git'

Plug 'https://github.com/ycm-core/YouCompleteMe.git'

Plug 'https://github.com/itchyny/lightline.vim.git'

Plug 'https://github.com/airblade/vim-gitgutter.git'

Plug 'https://github.com/vim-scripts/json-formatter.vim.git'

Plug 'https://github.com/janko/vim-test.git'

Plug 'https://github.com/joshdick/onedark.vim.git'

Plug 'https://github.com/Chiel92/vim-autoformat.git'

" F# plugins 
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }

Plug 'ionide/Ionide-vim', {
      \ 'do':  'make fsautocomplete',
      \}

Plug 'https://github.com/pseewald/vim-anyfold.git'

Plug 'christoomey/vim-tmux-runner'

Plug 'https://github.com/SirVer/ultisnips.git'

Plug 'dracula/vim', { 'as': 'dracula' }

Plug 'https://github.com/easymotion/vim-easymotion.git'

call plug#end()

set laststatus=2

set foldlevel=99 " Open all folds

let g:auto_save = 0

let g:auto_save_no_updatetime = 1

let g:auto_save_in_insert_mode = 0

let g:ycm_filetype_blacklist = {'cs': 1}

set updatetime=200

let g:easytags_async = 1

let g:OmniSharp_server_stdio = 1

let g:ale_linters = {
\ 'cs': ['OmniSharp']
\}

let g:ale_echo_cursor = 0

let g:OmniSharp_highlight_types = 2

let g:OmniSharp_selector_ui = 'fzf'

let g:NERDTreeWinSize=60

let test#csharp#runner = 'dotnettest'

let test#strategy = 'vtr'

let g:VtrPercentage = 35

" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<c-g>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

"if (empty($TMUX))
"  if (has("nvim"))
"    "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
"    let $NVIM_TUI_ENABLE_TRUE_COLOR=1
"  endif
"  "For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
"  "Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
"  " < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
"  if (has("termguicolors"))
"    set termguicolors
"  endif
"endif

colorscheme onedark
hi! Normal guibg=NONE ctermbg=NONE

" Omnisharp configs

augroup omnisharp_commands
    autocmd!

    " Show type information automatically when the cursor stops moving
    "autocmd CursorHold *.cs call OmniSharp#TypeLookupWithoutDocumentation()

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
nnoremap <Leader>ss :OmniSharpStartServer<CR>
nnoremap <Leader>sp :OmniSharpStopServer<CR>
nnoremap <Leader>sr :OmniSharpRestartServer<CR>
nnoremap <Leader>ad :ALEDetail<CR>
nnoremap <Leader><Leader>fd :Files!<CR>
nnoremap <Leader>fd :GFiles!<CR>
noremap <Leader>uh :GitGutterUndoHunk<CR>
noremap <Leader>nt :NERDTreeFind<CR>
noremap <Leader>fb :Buffers<CR>
noremap <Leader>an :ALENext<CR>
noremap <Leader>rg :Rg!<CR>
noremap <Leader>ed :call EmptyDiff("

" Git mappings 
nmap <Leader>gs :Gstatus<CR>
nmap <Leader>gl :Glog<CR>
nmap <Leader>gd :Gvdiffsplit<CR>

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

" Easy motion settings
let g:EasyMotion_smartcase = 1
nmap s <Plug>(easymotion-overwin-f2)

