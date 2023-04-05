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

Code navigation

- [x] Jump to definition
- [x] Find references (only in open files)
- [x] Folding range
- [ ] Selection range
- [x] Jump to type definition
- [ ] Document symbols
- [x] Document links
- [ ] Workspace symbols

Diagnostics

- [x] Parser diagnostics
- [x] Type-checker diagnostics

Code editing

- [x] Hovers
- [x] Rename symbol (only in open files)
- [ ] Code completion for variable names
- [ ] Code completion for record fields and constructors
- [ ] Signature help
- [ ] Refactorings

Formatting

- [x] Whole document formatting
- [ ] On-type formatting
- [x] Document range formatting

## Commands

You can restart the LSP server executing the `LIGO: LIGO Restart LSP Server` command. Likewise, commands to start and stop the server are supported as well.

LIGO Options contains various commands for building and running LIGO functions and expressions.

Deploy LIGO contains options to support deploying and generating deploy scripts.
