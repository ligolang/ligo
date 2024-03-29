
### SYNOPSIS
```
ligo compile type TYPE_EXPRESSION
```

### DESCRIPTION
This sub-command compiles a LIGO type to a Michelson type value.

### FLAGS
**--display-format FORMAT**
the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile. (alias: --format)

**--init-file FILENAME**
the path to the smart contract file to be used for context initialization.

**--library LIBS**
A comma-separated list of paths to directories where to search for files to be included by the preprocessor (alias: -l)

**--michelson-format CODE_FORMAT**
format that will be used by compile-contract for the resulting Michelson. Available formats are 'text' (default), 'json', 'msgpack' and 'hex'.

**--no-color**
disable coloring in CLI output

**--no-stdlib**
disable stdlib inclusion.

**--no-warn**
disable warning messages

**--project-root PATH**
The path to root of the project.

**--skip-analytics**
Avoid ligo analytics publication. Configurable with environment variable LIGO_SKIP_ANALYTICS too

**--syntax SYNTAX**
the syntax that will be used. Currently supported syntaxes are "cameligo" and "jsligo". By default, the syntax is guessed from the extension (.mligo and .jsligo respectively). (alias: -s)

**--warn-infinite-loop**
warn about infinite loop

**--warn-unused-rec**
warn about unused recursion in a recursive function

**--werror**
treat warnings as errors

**-D pass**
a list of defines to the preprocessor

**-p PROTOCOL**
choose protocol's types/values pre-loaded into the LIGO environment (oxford2). By default, the current protocol (oxford2) will be used (alias: --protocol)

**-help**
print this help text and exit (alias: -?)


