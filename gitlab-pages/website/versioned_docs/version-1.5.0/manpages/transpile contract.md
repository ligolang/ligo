
### SYNOPSIS
```
ligo transpile contract SOURCE_FILE
```

### DESCRIPTION
This sub-command transpiles a source file to another syntax.It parses the source file and performs the transpiling at the syntactic level.It can be used for transpiling PascaLIGO contracts to JsLIGO.

### FLAGS
**--display-format FORMAT**
the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile. (alias: --format)

**--from-syntax SYNTAX**
the syntax to the transpilation input.

**--no-color**
disable coloring in CLI output

**--output-file FILENAME**
if used, prints the output into the specified file instead of stdout (alias: -o)

**--skip-analytics**
Avoid ligo analytics publication. Configurable with environment variable LIGO_SKIP_ANALYTICS too

**--to-syntax SYNTAX**
the syntax to the transpilation output.

**-help**
print this help text and exit (alias: -?)


