
### SYNOPSIS
```
ligo run interpret EXPRESSION_EXPRESSION
```

### DESCRIPTION
This sub-command interprets a LIGO expression. The context can be initialized by providing a source file. The interpretation is done using Michelson's interpreter.

### FLAGS
**--amount INT**
the tezos amount the Michelson interpreter will use for the transaction.

**--balance INT**
the balance the Michelson interpreter will use for the contract balance.

**--display-format FORMAT**
the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile. (alias: --format)

**--init-file FILENAME**
the path to the smart contract file to be used for context initialization.

**--library LIBS**
A comma-separated list of paths to directories where to search for files to be included by the preprocessor (alias: -l)

**--no-color**
disable coloring in CLI output

**--now TIMESTAMP**
the NOW value the Michelson interpreter will use (e.g. '2000-01-01T10:10:10Z')

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

**-p PROTOCOL**
choose protocol's types/values pre-loaded into the LIGO environment (mumbai , nairobi). By default, the current protocol (nairobi) will be used (alias: --protocol)

**-help**
print this help text and exit (alias: -?)


