# LIGO Debugger (Early Alpha Preview)

A Visual Studio Code extension that provides LIGO debugging capability.

**Note**: This package is in a very early alpha version, released as a preview.
Bugs are to be expected!

In the future, this package will be deprecated and joined with the LIGO Language Server (`ligo-vscode`).

To use this package, you need to have LIGO version 0.47.0 or greater.
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

Currently, simple contracts are supported, with support for more complex contracts on the way.

Due to some limitations on `ligo` executable's side and on our side an invalid `Michelson` code may be produced. If you encounter this problem, please contact us.

We slightly change the `Michelson` code, which may result in minor differences (e.g. `PACK`ed code may result in a different bytestring).

At this moment contracts with tickets have very limited support.

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

It is possible to hardcode a concrete value both in LIGO and Michelson. You just need to suffix it with `@` and a preffered format. For example:
```json
...
"parameter": "5n@LIGO", <-- value in LIGO format
"storage": "Left 42@Michelson" <-- value in Michelson format
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

## FAQ

### I've set `"entrypoint": "{AskOnStart}"` in the configuration, and I'm still not asked for an entrypoint when starting a debug session.

We automatically detect the list of entrypoints in the file, and in case it contains only one entrypoint, we skip the selection stage.
Make sure that your function is a valid entrypoint.

### I've set `"michelsonEntrypoint": "{AskOnStart}"` in the configuration, and I'm still not asked for a Michelson entrypoint when starting a debug session.

We automatically detect the list of entrypoints in the contract, and in case it contains only one entrypoint, we skip the selection stage.
