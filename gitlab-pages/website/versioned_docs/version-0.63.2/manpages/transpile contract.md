
### SYNOPSIS
```
ligo transpile contract SOURCE_FILE SYNTAX
```

### DESCRIPTION
This sub-command transpiles a source file to another syntax.It parses the source file and performs the transpiling at the syntactic level.It can be used for transpiling PascaLIGO contracts to JsLIGO.

### FLAGS
**--display-format FORMAT**
the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile. (alias: --format)

**--no-color**
disable coloring in CLI output

**--output-file FILENAME**
if used, prints the output into the specified file instead of stdout (alias: -o)

**--syntax SYNTAX**
the syntax that will be used. Currently supported syntaxes are "cameligo" and "jsligo". By default, the syntax is guessed from the extension (.mligo and .jsligo respectively). (alias: -s)

**-help**
print this help text and exit (alias: -?)


