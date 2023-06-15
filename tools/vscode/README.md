# LIGO VS Code Plugin

This plugin is an LSP implementation for the LIGO language family.

Currently, it is highly experimental and may contain bugs.
Language Server capabilities on Windows are supported only if running in WSL mode.

Note: You need a LIGO build with support for `ligo lsp`.
LIGO version 0.61.0 and greater will come with support for language server capabilities.

To report bugs in the LIGO Language Server (LLS), please open an issue in [GitLab](https://gitlab.com/ligolang/ligo/-/issues).

Version 0.5.0 brings a rewrite of the language server in OCaml to improve the perfomance, stability, and more functionalities implemented in the LIGO compiler.

However, this rewrite doesn't have feature parity yet with the old versions (0.4.29 and older).
Those are being actively worked on.

## Functionality

### Code navigation

- [x] Jump to definition
- [x] Find references (only in open files)
- [x] Folding range
- [ ] Selection range
- [x] Jump to type definition
- [ ] Document symbols
- [x] Document links
- [ ] Workspace symbols

### Diagnostics

- [x] Parser diagnostics
- [x] Type-checker diagnostics

### Code editing

- [x] Hovers
- [x] Rename symbol (only in open files)
- [ ] Signature help
- [ ] Refactorings

### Code completion

- [X] Variable, module and type names
- [X] Record fields
- [X] Module fields
- [X] Keywords and operators
- [ ] Constructors
- [ ] Files
- [ ] Type-aware code completion

### Formatting

- [x] Whole document formatting
- [ ] On-type formatting
- [x] Document range formatting (BETA)

It can be configured via extension settings or by creating a `.ligopretty` file in the project root.

A `.ligopretty` file should contain a JSON object with

- `printWidth : int`- max line size (in characters) for file after pretty printing, default is 80.
- `tabWidth : int`- ident size, default is editor's tab size. Currently supported only for JsLIGO.

All fields are optional. If some option is specified both in a `.ligopretty` file and
in the `Ligo Language Server: Max Line Width` extension setting, the option from `.ligopretty` file would be used.

## Commands

You can restart the LSP server by executing the `LIGO: LIGO Restart LSP Server` command. Likewise, commands to start and stop the server are supported as well.

LIGO Options contains various commands for building and running LIGO functions and expressions.

Deploy LIGO contains options to support deploying and generating deploy scripts.

## Enabling and disabling features

The extension supports disabling specific LSP features. To do that, add the following in your `settings.json`:

```json
"ligoLanguageServer.disabledFeatures": [
]
```

Inside the list, you can write the name of any capability to disable it. For example, to disable formatting:

```json
"ligoLanguageServer.disabledFeatures": [
   "textDocument/formatting"
]
```

The supported features that may be disabled are listed below:

- `textDocument/definition`
- `textDocument/typeDefinition`
- `textDocument/references`
- `textDocument/completion`
- `textDocument/signatureHelp`
- `textDocument/foldingRange`
- `textDocument/selectionRange`
- `textDocument/documentLink`
- `textDocument/documentSymbol`
- `textDocument/hover`
- `textDocument/rename`
- `textDocument/prepareRename`
- `textDocument/formatting`
- `textDocument/rangeFormatting`
- `textDocument/codeAction`

**Note**: Please restart the LIGO Language Server after changing this configuration.
