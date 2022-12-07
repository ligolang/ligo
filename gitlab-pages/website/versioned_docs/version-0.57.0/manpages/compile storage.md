
### SYNOPSIS
```
ligo compile storage SOURCE_FILE STORAGE_EXPRESSION
```

### DESCRIPTION
This sub-command compiles an initial storage for a given contract to a Michelson expression. The resulting Michelson expression can be passed as an argument in a transaction which originates a contract.

### FLAGS
**--amount INT**
the tezos amount the Michelson interpreter will use for the transaction.

**--balance INT**
the balance the Michelson interpreter will use for the contract balance.

**--constants CONSTANTS**
A list of global constants that will be assumed in the context, separated by ',' (alias: -c)

**--display-format FORMAT**
the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile. (alias: --format)

**--file-constants FILE_CONSTANTS**
A file with a JSON list of strings with Michelson code. Those Michelson values will be registered as global constants in the context.

**--michelson-format CODE_FORMAT**
format that will be used by compile-contract for the resulting Michelson. Available formats are 'text' (default), 'json' and 'hex'.

**--no-warn**
disable warning messages

**--now TIMESTAMP**
the NOW value the Michelson interpreter will use (e.g. '2000-01-01T10:10:10Z')

**--output-file FILENAME**
if used, prints the output into the specified file instead of stdout (alias: -o)

**--project-root PATH**
The path to root of the project.

**--sender ADDRESS**
the sender the Michelson interpreter transaction will use.

**--source ADDRESS**
the source the Michelson interpreter transaction will use.

**--syntax SYNTAX**
the syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo", "reasonligo" and "jsligo". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, and .jsligo respectively). (alias: -s)

**--warn-unused-rec**
warn about unused recursion in a recursive function

**--werror**
treat warnings as errors

**-e ENTRY-POINT**
the entry-point that will be compiled. (alias: --entry-point)

**-p PROTOCOL**
choose protocol's types/values pre-loaded into the LIGO environment (kathmandu , lima). By default, the current protocol (kathmandu) will be used (alias: --protocol)

**-help**
print this help text and exit (alias: -?)


