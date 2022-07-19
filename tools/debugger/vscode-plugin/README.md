# LIGO Debugger (Early Alpha Preview)

A Visual Studio Code extension that provides LIGO debugging capability.

**Note**: This package is in a very early alpha version, released as a preview.
Bugs are to be expected!

In the future, this package will be deprecated and joined with the LIGO Language Server (`ligo-vscode`).

**Important**: To use this package, you need a version of LIGO in your `PATH` with optimizations disabled. Such a version of LIGO can be built from [this GitLab branch](https://gitlab.com/ligolang/ligo/-/merge_requests/1800).

Currently, simple contracts are supported, with support for more complex contracts on the way.

## Running the debugger

You can press F5 to start debugging a LIGO contract. Upon the launch of the debugger, you will be asked for a value for the parameter, and a value for the storage. For now, you can provide a LIGO entrypoint in your `launch.json`, or the extension will default to `main` if none is provided.

Use F11 (or press "Step Into") to step through LIGO code in details.

## Launch configuration

### Passing parameter and storage

The `parameter` and `storage` fields define what will be passed as run arguments.

It is possible to hardcode a concrete value both in LIGO and Michelson. You just need to suffix it with `@` and a preffered format. For example:
```json
...
"parameter": "5n@LIGO", <-- value in LIGO format
"storage": "Left 42@Michelson" <-- value in Michelson format
...
```

However, usually you might prefer using the auto-generated values like `${command:AskForParameter}` that would request the actual value upon starting a debug session.

In both cases you can use complex expressions, e.g. `{ a = 1; b = 2 }` for record definition or even `let x = 9 in x * x + 5` in Cameligo.
The dialect of the passed expressions must match the dialect of the contract.

It is possible to refer to constants declared in the contract and even call functions.

In case you need to supply a value in the lower-level Michelson format, just use switch button in the top right corner of input box.

### Specifying a Michelson entrypoint

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
