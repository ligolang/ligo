# LIGO VS Code Plugin

This plugin is an LSP and Debugger implementations for the LIGO language family.

# LSP

Currently, it is highly experimental and may contain bugs.
Language Server capabilities on Windows are supported only if running in WSL mode.

Note: You need a LIGO build with support for `ligo lsp`.
LIGO version 0.61.0 and greater come with support for language server capabilities.

To report bugs in the LIGO Language Server (LLS), please open an issue in [GitLab](https://gitlab.com/ligolang/ligo/-/issues).
You should find a file called `ligo_language_server.log` in your temporary directory. For example, in Linux, this should be inside `/tmp/`, which might contain information that the devs would find useful to debug your problem.

Version 0.5.0 brings a rewrite of the language server in OCaml to improve the perfomance, stability, and more functionalities implemented in the LIGO compiler.

However, this rewrite doesn't have feature parity yet with the old versions (0.4.29 and older).
Those are being actively worked on.

## Functionality

### Code navigation

- [x] Jump to definition
- [x] Jump to implementation
- [x] Find references
- [x] Folding range
- [ ] Selection range
- [x] Jump to type definition
- [x] Document symbols
- [x] Document links
- [ ] Workspace symbols
- [x] Term occurrences highlight (your color theme should be aware of LSPs, for Neovim see [one example](https://github.com/folke/tokyonight.nvim) of such theme)

### Diagnostics

- [x] Parser diagnostics
- [x] Type-checker diagnostics

### Code editing

- [x] Hovers
- [x] Rename symbol
- [ ] Signature help
- [ ] Refactorings

### Code completion

- [X] Variable, module and type names
- [X] Record fields
- [X] Module fields
- [X] Keywords and operators
- [x] Constructors
- [x] Files
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
- `textDocument/implementation`
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
- `textDocument/semanticTokens/full`
- `textDocument/semanticTokens/range`
- `textDocument/documentHighlight`

**Note**: Please restart the LIGO Language Server after changing this configuration.

## Project indexing

The LIGO Language Server will look for the contents of the directory where the opened LIGO file is, as well as all its parent directories, in search for a `ligo.json` file. Supposing that `/home/johndoe/ligo/foo.mligo` is open, then the language server will look for `ligo.json` file in `/home/jonhdoe/ligo`, `/home/jonhdoe`, `/home`, and `/`, in this order, using the first encounter of `ligo.json`, if it exists.

If this file is not found, the language sever will ask the user to create it in the directory where the file is open.
Moreover, if an old `esy.json` or `.ligoproject` file is found, the language server might ask the user to create in these locations, if preferred.
Note that both `esy.json` and `.ligoproject` are deprecated and ignored, so `ligo.json` should be used instead.

If there is no `ligo.json`, some features such as importing from LIGO packages might not work.

You need to run `ligo install` in the directory with `ligo.json` to import libraries from the LIGO registry and to use them in the language server.

### Specification of `ligo.json`

The `ligo.json` file represents a JSON object which, for now, has no fields that are used by the language server.
Some other fields, such as `dependencies`, are still used by the LIGO compiler, but ignored by the language server.

In the future, we plan on expanding the functionality of this file by adding more fields.

# Debugger

## Connecting the `ligo` executable

You will need the `ligo` executable available on your computer. You can find out more from the [installation instructions](https://www.ligolang.org/docs/intro/installation/).

You can specify a path to the `ligo` executable in `settings.json`. The debugger will look for it in the following order:
1. The debugger will use this path if this field is filled.
2. If this field is blank, the debugger will try to find the `ligo` executable in the `$PATH` variable.
3. Otherwise, the debugger will use the path from the `$LIGO_BINARY_PATH` variable.

Also, if you prefer using `ligo` from the Docker image then you can specify a path to the next script which runs `ligo` from Docker:

```sh
#!/bin/sh
docker run --rm -v $(pwd):$(pwd) -w $(pwd) ligolang/ligo:{ligo-version} "$@"
```

where `{ligo-version}` is your preferred `ligo` version (e.g. `0.70.1`).

At this moment running `ligo` from Docker is slow, so it's better to use the static binary.

## Functionality support

What is supported as part of MVP:
* Contracts using the common functionality;
* Convenient supply of endpoint / parameter / storage with last value remembering and two input modes;
* All stepping commands (including `Step Back`) with statement and expression granularities;
* Display of variables, including records, constructors, lists, and combinations of them;
* Stack frames display;
* Breakpoints (but not guaranteed to work properly in all the cases at the moment);
* Providing custom environment (`Tezos.get_now`, `Tezos.get_balance`, etc).

Bits of functionality that will be added very soon:
* Contracts related functionality:
  - [ ] Running contract with embedded Michelson.
  - [ ] Running contract with other contracts origination.
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

You can press F5 to start debugging a LIGO contract. Upon the launch of the debugger, you will be asked for a value for the parameter, and a value for the storage. You can provide a LIGO module entrypoint in your `launch.json` with `(*@AskOnStart@*)`. It will ask you to choose an entrypoint for your contract. If you want to hardcode it, then you can write it in this field.
```json
"moduleName": "(*@AskOnStart@*)" <-- will ask you to choose an entrypoint via quickpick
```
```json
"moduleName": "Main1" <-- will use "Main1" as module entrypoint
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

However, usually you might prefer using the auto-filled values like `(*@AskOnStart@*)` that would request the actual value upon starting a debug session.

In both cases you can use complex expressions, e.g. `{ a = 1; b = 2 }` for record definition or even `let x = 9 in x * x + 5` in Cameligo.
The dialect of the passed expressions must match the dialect of the contract.

It is possible to refer to constants declared in the contract and even call functions.

In case you need to supply a value in the lower-level Michelson format, just use switch button in the top right corner of input box.

### Specifying a LIGO entrypoint

Entrypoints in LIGO are marked with `@entry` annotation. For example:
```ocaml
[@entry]
let increment (a : int) (st : storage) : ret = ...

[@entry]
let decrement (a : int) (st : storage) : ret = ...
```
If you want to debug a specific entrypoint then you can use `entrypoint` field in `launch.json`. Example:
```json
{
  "entrypoint": "increment",
  "parameter": 42
}
```
You can omit it or use `(*@AskOnStart@*)`. In both cases, it will ask you for an entrypoint if your contract has more than one.

### Passing a custom environment
The debugger supports providing a custom environment for your contracts. You can customize it in the `contractEnv` field. An example of configuration:
```json
...
"contractEnv": {
  "now": "2020-01-01T00:00:00Z",
  "level": "10000",
  "sender": "tz1hTK4RYECTKcjp2dddQuRGUX5Lhse3kPNY",
  "source": "tz1hTK4RYECTKcjp2dddQuRGUX5Lhse3kPNY",
  "self": "KT1XQcegsEtio9oGbLUHA8SKX4iZ2rpEXY9b",
  "amount": "0",
  "balance": "1000000",
  "chainId": "NetXH12Aer3be93",
  "votingPowers": {
    "kind": "simple",
    "contents": {
      "tz1aZcxeRT4DDZZkYcU3vuBaaBRtnxyTmQRr": "100"
    }
  }
}
...
```
All these fields are optional. Let's describe what they mean:
1. `now`. The value returned by `Tezos.get_now()`. Default: current system time.
2. `level`. The value returned by `Tezos.get_level()`. Default: `"10000"`.
3. `sender`. The value returned by `Tezos.get_sender()`. Default: `"tz1hTK4RYECTKcjp2dddQuRGUX5Lhse3kPNY"`.
4. `source`. The value returned by `Tezos.get_source()`. Default: `"tz1hTK4RYECTKcjp2dddQuRGUX5Lhse3kPNY"`.
5. `self`. The value returned by `Tezos.get_self_address()`. Default: `"KT1XQcegsEtio9oGbLUHA8SKX4iZ2rpEXY9b"`.
6. `amount`. The value returned by `Tezos.get_amount()`. Default: `"0"`.
7. `balance`. The value returned by `Tezos.get_balance()`. Default: `"1000000"`.
8. `chainId`. The value returned by `Tezos.get_chain_id()`. Default: `"NetXH12Aer3be93"`.
9. `votingPowers`. At this moment only the `simple` kind is supported. In the `contents` field you should specify key hashes and their voting powers. Default: `{ "kind": "simple", "contents": { "tz1aZcxeRT4DDZZkYcU3vuBaaBRtnxyTmQRr": "100" } }`.

### Providing configuration in LIGO
You can also provide a configuration using one of the LIGO dialects. It could be done by using `Debug: Create configuration in LIGO for the debugger` command (`Ctrl + Shift + P` shortcut for opening the command palette). It will ask you for a directory where the config would be created and for a config name. Note, that it will **overwrite** the file with the same name.

An example of the config (in `CameLIGO`):
```ocaml
let contract_env =
  { now           = "2020-01-01T00:00:00Z"
  ; balance       = 1tez
  ; amount        = 2tez
  ; self          = "KT1XQcegsEtio9oGbLUHA8SKX4iZ2rpEXY9b"
  ; source        = "tz1hTK4RYECTKcjp2dddQuRGUX5Lhse3kPNY"
  ; sender        = "tz1hTK4RYECTKcjp2dddQuRGUX5Lhse3kPNY"
  ; chain_id      = "NetXH12Aer3be93"
  ; level         = 10000
  ; voting_powers = Map.literal
      [ "tz1aZcxeRT4DDZZkYcU3vuBaaBRtnxyTmQRr", 40
      ; "tz1hTK4RYECTKcjp2dddQuRGUX5Lhse3kPNY", 60
      ]
  }

let config =
  { parameter    = "*parameter value*"
  ; storage      = "*storage value*"
  ; program      = "*path to program*"
  ; module_name  = "*module name*"
  ; entrypoint   = "*entrypoint name*"
  ; log_dir      = "*log directory*"
  ; contract_env = contract_env
  }
```
Note, that all these fields are optional and you can omit them. The debugger will ask for them if they're needed (e.g. an entrypoint or parameter). The only required object here is `config`.

**Don't forget to add a path to the LIGO config into your `launch.json` configuration (`configPath` field). The debugger doesn't do this automatically. You can set (\*@AskOnStart@\*) command there and the debugger will ask you for a config path.**

Also, the LIGO configuration will overlap the existing `launch.json` one. For example, if you haven't specified the `now` field in the `contract_env` but you have it in the `launch.json`, then it will be set to default value.

## Stepping

Use the available `Next`, `StepIn`, and other commands to go over the contract execution.
Stepping back is supported too.

You can change the stepping granularity with a button at the lower panel:

![Granularity selection button](./docs/stepping-granularity-selection.png)

which is useful for evaluating complex expressions step-by-step.

Use `Ctrl + F10` (`Cmd + F10` on Mac) to switch between the last and currently selected granularities.

## FAQ

### I've set `"moduleName": "(*@AskOnStart@*)"` in the configuration, and I'm still not asked for a module name when starting a debug session.

We automatically detect the list of module names in the file, and in case it contains only one, we skip the selection stage.
Make sure that your module name is a valid one.

### I've set `"entrypoint": "(*@AskOnStart@*)"` in the configuration, and I'm still not asked for an entrypoint when starting a debug session.

We automatically detect the list of entrypoints in the contract, and in case it contains only one entrypoint, we skip the selection stage.

### Debugger stepping order is weird

Before being executed, LIGO contracts are converted to Michelson language (which all Tezos smart contracts are eventually written in), and generally some things get reordered to get a more optimal contract.

For example, when executing binary operations, the right operand is computed first.

In debugger we try to preserve the execution order, otherwise runs in production and in debugger could have different outcomes. As example of this change in result imagine a binary operation, both sides of which result in a failure.

However, some reorderings can be safely illiminated. We try to handle such cases accordingly, but if you think this is your case, we would appreciate if you reported this issue.

### I see many instances of the same function/value in the variables pane

It's okay. At this moment debugger doesn't support polymorphic types in the variables pane, so, you'll see all monomorphed variants that are used in your contract.

### Debugger says that my contract has an infinite loop but it doesn't

By default the debugger will do 1000 steps and after that will assume that a contract has an infinite loop. You can change the max steps value in the settings. By _step_ we mean step in `expression (pre + post)` granularity.

### From some moment I see `%field% {AskOnStart} does not exist` error
We changed command braces from `{` and `}` to `(*@` and `@*)` because the previous ones were conflicting with LIGO record notation. So, now you need to write `(*@AskOnStart@*)` instead of `{AskOnStart}`.

### Config resolution fails with a type error but LSP doesn't highlight them
It could be a problem with polymorphic types in `"parameter"` and/or `"storage"` fields. All the types should be monomorphized (i.e. there're no `forall` types) on the config resolution stage.

If it's not your case then, please, contact us.
