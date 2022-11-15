# Debugger

[![VSCode Extension](https://vsmarketplacebadge.apphb.com/version/ligolang-publish.ligo-debugger-vscode.svg?label=VSCode%20Extension)](https://marketplace.visualstudio.com/items?itemName=ligolang-publish.ligo-debugger-vscode)

A debugger for LIGO contracts for VSCode IDE.

It consists of two parts:

* Haskell backend in [`ligo-debugger`](./ligo-debugger) folder.
* VSCode extension in [`vscode-plugin`](./vscode-plugin) folder.

See [`vscode-plugin/README.md`](./vscode-plugin/README.md) for more detailed instructions on how to launch and use the debugger.

## How to build

To build the plugin, run `make package`; this will add `.vsix` file to `vscode-plugin` folder.
You can then use this file to install the extension from VSCode interface:

To build and install the plugin at once, run `make install-plugin`.
If this is not the first time you install the plugin, you may need to reload VSCode manually.

