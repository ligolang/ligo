# Debugger

A debugger for LIGO contracts for VSCode IDE.

It consists of two parts:

* Haskell backend in [`ligo-debugger`](./ligo-debugger) folder;
* VSCode extension in [`vscode-plugin`](./vscode-plugin) folder.


## How to build

To build the plugin, run `make package`; this will add `.vsix` file to `vscode-plugin` folder.
You can then use this file to install the extension from VSCode interface:

To build and install the plugin at once, run `make install-plugin`.
If this is not the first time you install the plugin, you may need to reload VSCode manually.


## Launch configuration

### Passing parameter and storage

The `parameter` and `storage` fields define what will be passed as run arguments.

It is possible to hardcode a concrete value.
However, usually you might prefer using the auto-generated values like `${command:AskForParameter}` that would request the actual value upon starting a debug session.

In both cases you can use complex expressions, e.g. `{ a = 1; b = 2 }` for record definition or even `let x = 9 in x * x + 5` in Cameligo.
The dialect of the passed expressions must match the dialect of the contract.

It is possible to refer to constants declared in the contract and even call functions.

In case you need to supply a value in the lower-level Michelson format, prefix it with `michelson:` or just `m:`

```
m:Pair 1 "a"
```

### Specifying michelson entrypoint

When the parameter of your contract has multiple constructors, normally you can just pass something like `Constructor1 5` as a parameter.

In case of nested constructors, it might be simpler to specify the bottom-most constructor name (which generally must be unique across the contract) and its argument.
The underlying Michelson engine allows for this.

To make it work, set `michelsonEntrypoint` field to the entrypoint name.
Example:

```ocaml
type subparameterX =
  | CallX1 of int
  | CallX2 of string

type parameter =
  | CallX of subparameterX
  | CallY
```

With such a contract, you can specify in `launch.json`:

```json
{
    "michelsonEntrypoint": "CallX1",
    "parameter": 5
}
```