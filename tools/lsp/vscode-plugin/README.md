# LIGO VSCode Plugin

This plugin is an LSP implementation for the LIGO language family.

Currently, it is highly experimental and may contain bugs. Language Server capabilities on Windows are supported only if running in WSL mode.

To report bugs in the LIGO Language Server (LLS), please open an issue in [GitLab](https://gitlab.com/serokell/ligo/ligo/-/issues). Additional information may be found in `/tmp/ligo-language-server.log`.

## Functionality
Code navigation

- [x] Jump to definition
- [x] Find references
- [x] Folding range
- [x] Selection range
- [x] Jump to type definition (limited)
- [x] Document symbols
- [x] Document links
- [ ] Workspace symbols

Diagnostics

- [x] Parser diagnostics
- [x] Compiler diagnostics (if LIGO is available in PATH)

Code editing

- [x] Hover suggestions
- [x] Rename symbol
- [x] Code completion for variable names
- [x] Code completion for record fields and constructors (limited)
- [x] Signature help
- [ ] Refactorings

Formatting

- [x] Whole document formatting
- [ ] On-type formatting
- [ ] Document range formatting

## Commands

You can restart the LSP server executing the `LIGO: LIGO Restart LSP Server` command. Likewise, commands to start and stop the server are supported as well.

## Enabling and disabling features

The extension supports disabling specific LSP features. To do that, add the following in your `settings.json`:

```json
"ligoLanguageServer.disabledFeatures": [
]
```

Inside the list, you can write the name of any capability to disable it. For example, to disable hovers:

```json
"ligoLanguageServer.disabledFeatures": [
   "textDocument/hover"
]
```

The supported features that may be disabled are listed below:
* `textDocument/definition`
* `textDocument/typeDefinition`
* `textDocument/references`
* `textDocument/completion`
* `textDocument/signatureHelp`
* `textDocument/foldingRange`
* `textDocument/selectionRange`
* `textDocument/documentLink`
* `textDocument/documentSymbol`
* `textDocument/hover`
* `textDocument/rename`
* `textDocument/prepareRename`
* `textDocument/formatting`
* `textDocument/rangeFormatting`
* `textDocument/codeAction`

## Releasing the plugin

Once the plugin is ready for release, code should be pushed to the `vscode-production` branch.
`vscode-extension-publish` job should be triggered manually from this branch pipeline.

You can read more about manual interaction with the pipeline [here](https://docs.gitlab.com/ee/ci/pipelines/#add-manual-interaction-to-your-pipeline).
