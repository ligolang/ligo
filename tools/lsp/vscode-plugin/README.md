# LIGO VSCode Plugin

This plugin is an LSP implementation for the LIGO language family.

Currently, it is highly experimental and may contain bugs. Language Server capabilities on Windows are supported only if running in WSL mode.

## Functionality
Code navigation

- [x] Jump to definition
- [x] Find references
- [x] Folding range
- [x] Selection range
- [ ] Jump to type definition
- [ ] Document symbols
- [ ] Workspace symbols

Diagnostics

- [x] Parser diagnostics
- [ ] Extended diagnostics

Code editing

- [x] Hover suggestions
- [ ] Code actions (refactor, rename, etc.)
- [x] Code completion for variable names
- [ ] Code completion for record fields and constructors
- [ ] Signature help

Formatting

- [ ] Whole document formatting
- [ ] On-type formatting
- [ ] Document range formatting

## Releasing the plugin

Once the plugin is ready for release, code should be pushed to the `vscode-production` branch.
`vscode-extension-publish` job should be triggered manually from this branch pipeline.

You can read more about manual interaction with the pipeline [here](https://docs.gitlab.com/ee/ci/pipelines/#add-manual-interaction-to-your-pipeline).
