### NAME

ligo

### SYNOPSIS

**ligo** *COMMAND* \...

### MORE HELP

Use \`**ligo** *COMMAND* \--help\` for help on a single command.

### DOCUMENTATION

https://ligolang.org/docs/intro/introduction

### ASK A QUESTION

https://discord.gg/9rhYaEt

### OPEN AN ISSUE

https://gitlab.com/ligolang/ligo/issues/new

### COMMANDS

**changelog**

:   Dump the LIGO changelog to stdout.

**compile-contract**

:   Subcommand: Compile a contract.

**compile-expression**

:   Subcommand: Compile to a Michelson value.

**compile-parameter**

:   Subcommand: Compile parameters to a Michelson expression.

**compile-storage**

:   Subcommand: Compile an initial storage in LIGO syntax to a Michelson
    expression.

**dry-run**

:   Subcommand: Run a smart-contract with the given storage and input.

**evaluate-call**

:   Subcommand: Run a function with the given parameter.

**evaluate-expr**

:   Subcommand: Evaluate a given definition.

**evaluate-value**

:   Deprecated, renamed to evaluate-expr. Use evaluate-expr instead.
    Subcommand: Evaluate a given definition.

**get-scope**

:   Subcommand: Return the JSON encoded environment for a given file.

**interpret**

:   Subcommand: Interpret the expression in the context initialized by
    the provided source file.

**list-declarations**

:   Subcommand: List all the top-level declarations.

**measure-contract**

:   Subcommand: Measure a contract\`s compiled size in bytes.

**preprocess**

:   Subcommand: Preprocess the source file. Warning: Intended for
    development of LIGO and can break at any time.

**pretty-print**

:   Subcommand: Pretty-print the source file.

**print-ast**

:   Subcommand: Print the AST. Warning: Intended for development of LIGO
    and can break at any time.

**print-ast-combined**

:   Subcommand: Print the contract after combination with the build
    system. Warning: Intended for development of LIGO and can break at
    any time.

**print-ast-core**

:   Subcommand: Print the AST. Warning: Intended for development of LIGO
    and can break at any time.

**print-ast-sugar**

:   Subcommand: Print the AST. Warning: Intended for development of LIGO
    and can break at any time.

**print-ast-typed**

:   Subcommand: Print the typed AST. Warning: Intended for development
    of LIGO and can break at any time.

**print-cst**

:   Subcommand: Print the CST. Warning: Intended for development of LIGO
    and can break at any time.

**print-graph**

:   Subcommand: Print the dependency graph. Warning: Intended for
    development of LIGO and can break at any time.

**print-mini-c**

:   Subcommand: Print Mini-C. Warning: Intended for development of LIGO
    and can break at any time.

**repl**

:   Subcommand: REPL

**run-function**

:   Deprecated, renamed to evaluate-call. Use evaluate-call instead.
    Subcommand: Run a function with the given parameter.

**test**

:   Subcommand: Test a contract with the LIGO test framework (BETA).

**transpile-contract**

:   Subcommand: Transpile a contract to another syntax (BETA).

**transpile-expression**

:   Subcommand: Transpile an expression to another syntax (BETA).

### OPTIONS

**\--help**\[=*FMT*\] (default=auto)

:   Show this help in format *FMT*. The value *FMT* must be one of
    \`auto\`, \`pager\`, \`groff\` or \`plain\`. With \`auto\`, the
    format is \`pager\` or \`plain\` whenever the **TERM** env var is
    \`dumb\` or undefined.

**\--version**

:   Show version information.
