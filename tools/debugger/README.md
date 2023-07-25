# Debugger

[![VSCode Extension](https://vsmarketplacebadge.apphb.com/version/ligolang-publish.ligo-debugger-vscode.svg?label=VSCode%20Extension)](https://marketplace.visualstudio.com/items?itemName=ligolang-publish.ligo-debugger-vscode)

A debugger for LIGO contracts for VSCode IDE.

It consists of two parts:

* Haskell backend in [`ligo-debugger`](./ligo-debugger) folder.
* VSCode extension in [`vscode-plugin`](./vscode-plugin) folder.

See [`vscode-plugin/README.md`](./vscode-plugin/README.md) for more detailed instructions on how to launch and use the debugger.

## How to build

### Build dependencies

You will need to have a few dependencies on your computer to build the debugger. To build the debug adapter:
* [Stack](https://docs.haskellstack.org/en/stable/) (we recommend using [GHCup](https://www.haskell.org/ghcup/) to get it)
* [`libsodium`](https://doc.libsodium.org)
* [`zlib`](https://zlib.net)
* [`tree-sitter`](https://www.npmjs.com/package/tree-sitter)
* [`tree-sitter-cli`](https://www.npmjs.com/package/tree-sitter-cli)

To build the Visual Studio Code extension:
* [`yarn`](https://yarnpkg.com)

### Build

To build the plugin, run `make package`; this will add `.vsix` file to `vscode-plugin` folder.
You can then use this file to install the extension from VSCode interface:

To build and install the plugin at once, run `make install-plugin`.
If this is not the first time you install the plugin, you may need to reload VSCode manually.

As a shortcut, you may run `make install-plugin` to package and install it.
