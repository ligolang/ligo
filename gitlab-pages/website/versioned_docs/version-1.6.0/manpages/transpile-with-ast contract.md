
### SYNOPSIS
```
ligo transpile-with-ast contract SOURCE_FILE SYNTAX
```

### DESCRIPTION
[BETA] This sub-command transpiles a source file to another syntax. It does not use the build system, but the source file is preprocessed. Comments are currently not transpiled. Please use at your own risk.

### FLAGS
**--display-format FORMAT**
the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile. (alias: --format)

**--library LIBS**
A comma-separated list of paths to directories where to search for files to be included by the preprocessor (alias: -l)

**--no-color**
disable coloring in CLI output

**--output-file FILENAME**
if used, prints the output into the specified file instead of stdout (alias: -o)

**--skip-analytics**
Avoid ligo analytics publication. Configurable with environment variable LIGO_SKIP_ANALYTICS too

**--syntax SYNTAX**
the syntax that will be used. Currently supported syntaxes are "cameligo" and "jsligo". By default, the syntax is guessed from the extension (.mligo and .jsligo respectively). (alias: -s)

**-help**
print this help text and exit (alias: -?)


