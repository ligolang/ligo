
### SYNOPSIS
```
ligo transpile-with-ast expression SYNTAX _EXPRESSION SYNTAX
```

### DESCRIPTION
[BETA] This sub-command transpiles a LIGO expression to another syntax. Comments are currently not transpiled. Please use at your own risk.

### FLAGS
**--display-format FORMAT**
the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile. (alias: --format)

**--library LIBS**
A comma-separated list of paths to directories where to search for files to be included by the preprocessor (alias: -l)

**--no-color**
disable coloring in CLI output

**--skip-analytics**
Avoid ligo analytics publication. Configurable with environment variable LIGO_SKIP_ANALYTICS too

**-help**
print this help text and exit (alias: -?)


