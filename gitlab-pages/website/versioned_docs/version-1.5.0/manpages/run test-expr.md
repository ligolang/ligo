
### SYNOPSIS
```
ligo run test-expr SYNTAX _EXPRESSION
```

### DESCRIPTION
This sub-command tests a LIGO contract using a LIGO interpreter. Still under development, there are features that are work in progress and are subject to change. No real test procedure should rely on this sub-command alone.

### FLAGS
**--arg EXPRESSION**
an expression passed to LIGO interpreter, accessible through variable 'cli_arg'

**--display-format FORMAT**
the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile. (alias: --format)

**--init-file FILENAME**
the path to the smart contract file to be used for context initialization.

**--library LIBS**
A comma-separated list of paths to directories where to search for files to be included by the preprocessor (alias: -l)

**--no-color**
disable coloring in CLI output

**--no-warn**
disable warning messages

**--project-root PATH**
The path to root of the project.

**--skip-analytics**
Avoid ligo analytics publication. Configurable with environment variable LIGO_SKIP_ANALYTICS too

**--steps INT**
a bound in the number of steps to be done by the interpreter. (alias: -n)

**--warn-infinite-loop**
warn about infinite loop

**--warn-unused-rec**
warn about unused recursion in a recursive function

**-help**
print this help text and exit (alias: -?)


