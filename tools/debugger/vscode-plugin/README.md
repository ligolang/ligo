# LIGO Debugger (Early Alpha Preview)

A Visual Studio Code extension that provides LIGO debugging capability.

**Note**: This package is in a very early alpha version, released as a preview.
Bugs are to be expected!

In the future, this package will be deprecated and joined with the LIGO Language Server (`ligo-vscode`).

## Connecting the `ligo` executable

You can specify a path to the `ligo` executable in `settings.json`. The debugger will look for it in the following order:
1. The debugger will use this path if this field is filled.
2. If this field is blank, the debugger will try to find the `ligo` executable in the `$PATH` variable.
3. Otherwise, the debugger will use the path from the `$LIGO_BINARY_PATH` variable.

Also, if you prefer using `ligo` from the docker image then you can specify a path to the next script which runs `ligo` from docker:
```sh
#!/bin/sh
docker run --rm -v $(pwd):$(pwd) -w $(pwd) ligolang/ligo:{ligo-version} "$@"
```
where `{ligo-version}` is your preferred `ligo` version (e.g. `0.50.0`).

At this moment running `ligo` from docker is slow, so, it's better to use static binary.

## Functionality support

What is supported as part of MVP:
* Contracts using the common functionality;
* Convenient supply of endpoint / parameter / storage with last value remembering and two input modes;
* All stepping commands (including `Step Back`) with statement and expression granularities;
* Basic variables display;
* Stack frames display;
* Breakpoints (but not guaranteed to work properly in all the cases at the moment).

Bits of functionality that will be added very soon:
* Contracts related functionality:
  - [ ] Running contract with embedded Michelson.
  - [ ] Running contract with other contracts origination.
  - [ ] Providing custom environment (`NOW`, `BALANCE`, e.t.c).
  - [ ] Running contracts with global constants.
  - [ ] Running corner cases of contracts with tickets (the current support is partial).
* Others:
  - [ ] Work with the downloaded packages.
  - [ ] Speed up debug session start (currently it takes long for relatively large contracts).

Next we will work on extending the core functionality step by step: add full breakpoints support, viewing inner parts of variables' values, simpler ways of starting a debug session, logpoints support, and many other improvements.

Also some notes:

* Due to some limitations on `ligo` executable's side and on our side, an invalid `Michelson` code may be produced. If you encounter this problem, please contact us.
* We slightly change the `Michelson` code, this may result in minor differences (e.g. `PACK`ed code may result in a different bytestring).

## Running the debugger

You can press F5 to start debugging a LIGO contract. Upon the launch of the debugger, you will be asked for a value for the parameter, and a value for the storage. You can provide a LIGO entrypoint in your `launch.json` with `{AskOnStart}`. It will ask you to choose an entrypoint for your contract. If you want to hardcode it, then you can write it in this field.
```json
"entrypoint": "{AskOnStart}" <-- will ask you to choose an entrypoint via quickpick
```
```json
"entrypoint": "main_1" <-- will use "main_1" as entrypoint
```

Use F11 (or press "Step Into") to step through LIGO code in details.

## Launch configuration

### Passing parameter and storage

The `parameter` and `storage` fields define what will be passed as run arguments.

It is possible to hardcode a concrete value both in LIGO and Michelson. You just need to specify its origin in `parameterLang` or `storageLang` fields. Note, that these fields are optional and the default value for them is `LIGO`. For example:
```json
...
"parameter": "5n",
"parameterLang": "LIGO", <-- this field is optional
"storage": "Left 42",
"storageLang": "Michelson",
...
```

However, usually you might prefer using the auto-filled values like `{AskOnStart}` that would request the actual value upon starting a debug session.

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

## Stepping

Use the available `Next`, `StepIn`, and other commands to go over the contract execution.
Stepping back is supported too.

You can change the stepping granularity with a button at the lower panel:

![Granularity selection button](./docs/stepping-granularity-selection.png)

which is useful for evaluating complex expressions step-by-step.

Use `Ctrl + F10` (`Cmd + F10` on Mac) to switch between the last and currently selected granularities.

## FAQ

### I've set `"entrypoint": "{AskOnStart}"` in the configuration, and I'm still not asked for an entrypoint when starting a debug session.

We automatically detect the list of entrypoints in the file, and in case it contains only one entrypoint, we skip the selection stage.
Make sure that your function is a valid entrypoint.

### I've set `"michelsonEntrypoint": "{AskOnStart}"` in the configuration, and I'm still not asked for a Michelson entrypoint when starting a debug session.

We automatically detect the list of entrypoints in the contract, and in case it contains only one entrypoint, we skip the selection stage.

### Debugger stepping order is weird

Before being executed, LIGO contracts are converted to Michelson language (which all Tezos smart contracts are eventually written in), and generally some things get reordered to get a more optimal contract.

For example, when executing binary operations, the right operand is computed first.

In debugger we try to preserve the execution order, otherwise runs in production and in debugger could have different outcomes. As example of this change in result imagine a binary operation, both sides of which result in a failure.

However, some reorderings can be safely illiminated. We try to handle such cases accordingly, but if you think this is your case, we would appreciate if you reported this issue.

### I see many instances of the same function/value in the variables pane

It's okay. At this moment debugger doesn't support polymorphic types in the variables pane, so, you'll see all monomorphed variants that are used in your contract.
