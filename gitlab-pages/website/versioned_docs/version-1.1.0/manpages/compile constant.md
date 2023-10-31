
### SYNOPSIS
```
ligo compile constant SYNTAX _EXPRESSION
```

### DESCRIPTION
This sub-command compiles a LIGO expression to a Michelson value and its hash as a global constant. It works by compiling the LIGO expression to a Michelson expression and then interpreting it using Michelson's interpreter.

### FLAGS
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

**--warn-infinite-loop**
warn about infinite loop

**--warn-unused-rec**
warn about unused recursion in a recursive function

**--werror**
treat warnings as errors

**--without-run**
disable running of compiled expression.

**-p PROTOCOL**
choose protocol's types/values pre-loaded into the LIGO environment (mumbai , nairobi). By default, the current protocol (nairobi) will be used (alias: --protocol)

**-help**
print this help text and exit (alias: -?)


