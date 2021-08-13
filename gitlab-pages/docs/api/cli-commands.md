---
id: cli-commands
title: CLI Commands
---

Contracts written in LIGO can be compiled using the `ligo` executable.


## Compiling a contract

Compile your contract with a specific entry point.

```zsh
ligo compile-contract SOURCE_FILE [ENTRY_POINT]
```

#### Example

```zsh
ligo compile-contract examples/counter.ligo main
```

## Defining the initial storage

If your contract implements a sophisticated storage, you can compile a LIGO expression into a Michelson value quite easily.

```zsh
ligo compile-storage SOURCE_FILE ENTRY_POINT EXPRESSION
```

#### Example
```zsh
ligo compile-storage examples/counter.ligo main 5
# Outputs: 5
```

## Invoking the contract with a parameter

```zsh
ligo compile-parameter SOURCE_FILE ENTRY_POINT EXPRESSION
```

#### Example
```zsh
ligo compile-parameter examples/counter.ligo main "Increment(5)"
# Outputs: (Right 5)
```