"""
" Add plugins here - this is the first call, because other configuration
" things deal with these plugins
call plug#begin()
Plug 'junegunn/goyo.vim'
Plug 'nanotech/jellybeans.vim'
Plug 'junegunn/limelight.vim'
Plug 'itchyny/lightline.vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'jalvesaq/zotcite'
Plug 'vim-pandoc/vim-markdownfootnotes/'
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'vim-pandoc/vim-pandoc-after'
Plug 'roxma/nvim-yarp'
call plug#end()

if has("vms")
  set nobackup		" do not keep a backup file, use versions instead
else
  set backup		" keep a backup file (restore to previous version)
  if has('persistent_undo')
    set undofile	" keep an undo file (undo changes after closing)
  endif
endif

" Removing backup directory
set nobackup
set nowritebackup

filetype plugin on

" Only do this part when compiled with support for autocommands.
if has("autocmd")

  " Put these in an autocmd group, so that we can delete them easily.
  augroup vimrcEx
  au!

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78

  augroup END

else

  set autoindent		" always set autoindenting on

endif " has("autocmd")
set number
set numberwidth=3
set relativenumber

"set colorscheme
colorscheme jellybeans

" CUSTOM KEYBINDINGS
" switch k and j to go up one visual line as opposed to one full code/gutter
" line
nnoremap k gk
nnoremap j gj

" break lines at the end of words, not at the wrapping columns
setlocal linebreak
setlocal breakat-=*

" let j and k move up and down final visual lines in document
setlocal display=lastline


" PLUGIN CONFIGURATION

" VIM-PANDOC PLUGIN
" set for all markdown files
let g:pandoc#filetypes#handled = ["pandoc", "markdown"]
     let g:pandoc#filetypes#pandoc_markdown = 0

" get rid of folding
let g:pandoc_no_folding = 1

" loading plugin integrations
let g:pandoc#after#modules#enabled = ["supertab"]


" set up local bibliography file, with path for understanding citekeys
" with g set, it is the globally detected and parsed file
" it also loads a value for the buffer that will let pandoc and SuperTab
" work on it
let g:pandoc#biblio#bibs = ['/home/anthony/Documents/academic/writing/.config/bibliography.bib']

" NCM2 (NEOVIM COMPLETION MANAGER) SETTINGS
" initial loading for markdown files in buffer
"augroup markdown
"  autocmd!
"  autocmd BufEnter * call ncm2#enable_for_buffer()
"  autocmd Filetype pandoc call ncm2#register_source({
"    \ 'name': 'pandoc',
"    \ 'priority': 8,
"    \ 'scope': ['pandoc', 'markdown'],
"    \ 'mark': 'md',
"    \ 'word_pattern': '\w+',
"    \ 'complete_pattern': ['@'],
"    \ 'on_complete': ['ncm2#on_complete#omni', 'pandoc#completion#Complete'],
"    \ })
"augroup END

" formatting the ncm completion to not show an obscene amount of information
"set completeopt=noinsert,menuone,noselect
"set shortmess+=c

" ZOTCITE SETTINGS

" SUPERTAB SETTINGS
" commenting out to see if it's conflicting with the zotero plugin
" let g:SuperTabDefaultCompletionType = "context"
" let g:SuperTabCompletionContexts = ['s:ContextText', 's:ContextDiscover']
" let g:SuperTabContextTextOmniPrecedence = ['&omnifunc', '&completefunc']

inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
