# LIGO language support for Vim and Neovim

This plugin adds support for JsLIGO and CameLIGO to Vim and Neovim.

Syntax highlighting is supported out-of-the box. Language features are provided
via LSP. `ligo` must be available in PATH.

## Installation

Simply copy or link the plugin to the `~/.vim/pack` directory:

```bash
mkdir -p ~/.vim/pack
cp -rv <path-to-ligo-repo>/tools/vim/ligo ~/.vim/pack/ligo
```

Or, if you are using Neovim, you might want to copy or link the plugin to `~/.local/share/nvim/site/pack`:

```bash
mkdir -p ~/.local/share/nvin/site/pack
cp -rv <path-to-ligo-repo>/tools/vim/ligo ~/.local/share/nvin/site/pack
```

## Language server configuration

You can use the following language client plugins:
* [`vim-lsp`](https://github.com/prabirshrestha/vim-lsp): preconfigured, ensure that `ligo` is on your vim $PATH,
   which you can check with `:echo $PATH`.
* [`LanguageClient-neovim`](https://github.com/autozimu/LanguageClient-neovim):
```vim
let g:LanguageClient_serverCommands = {
    \ 'ligo': ['path/to/ligo', 'lsp'],
    \ 'mligo': ['path/to/ligo', 'lsp'],
    \ 'jsligo': ['path/to/ligo', 'lsp'],
    \ }
```
