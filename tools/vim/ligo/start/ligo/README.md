# LIGO language support for Vim

This plugin adds support for CameLIGO to Vim.

Syntax highlighting is supported out-of-the box. Language features are provided
via LSP. `ligo` must be available in path.

## Installation
Simply copy the plugin to the `~/.vim/pack` directory:
```bash
mkdir -p ~/.vim/pack
cp -rv <path-to-ligo-repo>/tools/vim/ligo ~/.vim/pack/ligo
```

## Language server configuration

You can use the following language client plugins:
* `vim-lsp`: preconfigured, ensure that `ligo` is on your vim $PATH,
   which you can check with `:echo $PATH`.
* `coc.nvim`:
```json
"languageserver": {
  "Ligo": {
    "command": "path/to/ligo lsp",
    "filetypes": ["ligo", "mligo"]
  }
}
```

* `LanguageClient-neovim`:
```vim
let g:LanguageClient_serverCommands = {
    \ 'ligo': ['path/to/ligo', 'lsp'],
    \ 'mligo': ['path/to/ligo', 'lsp'],
    \ }
```
