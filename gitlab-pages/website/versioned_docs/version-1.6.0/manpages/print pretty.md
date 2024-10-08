
### SYNOPSIS
```
ligo print pretty SOURCE_FILE
```

### DESCRIPTION
This sub-command pretty-prints a source file in LIGO. The width of the pretty-printed text is adjusted to the number of columns in the terminal (or 60 if it cannot be determined).

### FLAGS
**--display-format FORMAT**
the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile. (alias: --format)

**--library LIBS**
A comma-separated list of paths to directories where to search for files to be included by the preprocessor (alias: -l)

**--no-color**
disable coloring in CLI output

**--parser-error-recovery**
Enable error-recovery in the parser.

**--project-root PATH**
The path to root of the project.

**--skip-analytics**
Avoid ligo analytics publication. Configurable with environment variable LIGO_SKIP_ANALYTICS too

**--syntax SYNTAX**
the syntax that will be used. Currently supported syntaxes are "cameligo" and "jsligo". By default, the syntax is guessed from the extension (.mligo and .jsligo respectively). (alias: -s)

**--werror**
treat warnings as errors

**-help**
print this help text and exit (alias: -?)


