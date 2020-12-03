# Emacs plugin for LIGO

This plugin features syntax highlighting and `lsp-mode` support for PascaLIGO, CameLIGO, and ReasonLIGO.

For the LSP to work, you need to install `lsp-mode` and put `ligo-squirrel` executable in PATH.

## Installation

Put `ligo-mode.el` to the emacs load path and add the following lines to your `init.el`:

```
(add-to-list 'load-path "<LIGO_MODE_DIR>")
(add-to-list 'auto-mode-alist '("\\.\\(re\\|m\\)?ligo\\'" . ligo-mode))
(autoload 'ligo-mode "ligo-mode" "LIGO mode" t)
```
