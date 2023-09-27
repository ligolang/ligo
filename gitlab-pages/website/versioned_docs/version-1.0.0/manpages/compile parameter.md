
### SYNOPSIS
```
ligo compile parameter SOURCE_FILE PARAMETER_EXPRESSION
```

### DESCRIPTION
This sub-command compiles a parameter for a given contract to a Michelson expression. The resulting Michelson expression can be passed as an argument in a transaction which calls a contract.

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

**--library LIBS**
A comma-separated list of paths to directories where to search for files to be included by the preprocessor (alias: -l)

**--michelson-format CODE_FORMAT**
format that will be used by compile-contract for the resulting Michelson. Available formats are 'text' (default), 'json', 'msgpack' and 'hex'.

**--no-color**
disable coloring in CLI output

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

**--skip-analytics**
Avoid ligo analytics publication. Configurable with environment variable LIGO_SKIP_ANALYTICS too

**--source ADDRESS**
the source the Michelson interpreter transaction will use.

**--syntax SYNTAX**
the syntax that will be used. Currently supported syntaxes are "cameligo" and "jsligo". By default, the syntax is guessed from the extension (.mligo and .jsligo respectively). (alias: -s)

**--warn-infinite-loop**
warn about infinite loop

**--warn-unused-rec**
warn about unused recursion in a recursive function

**--werror**
treat warnings as errors

**-e ENTRY-POINT**
the entry-point to be matched against the parameter expression (alias: --entry-point)

**-m MODULE**
the entry-point will be compiled from that module. (alias: --module)

**-p PROTOCOL**
choose protocol's types/values pre-loaded into the LIGO environment (mumbai , nairobi). By default, the current protocol (nairobi) will be used (alias: --protocol)

**-help**
print this help text and exit (alias: -?)


