
### SYNOPSIS
```
ligo print dependency-graph SOURCE_FILE
```

### DESCRIPTION
This sub-command prints the dependency graph created by the module system. It explores all imported source files (recursively) following a DFS strategy.

### FLAGS
**--display-format FORMAT**
the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile. (alias: --format)

**--no-colour**
disable coloring in CLI output

**--project-root PATH**
The path to root of the project.

**--syntax SYNTAX**
the syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo" and "jsligo". By default, the syntax is guessed from the extension (.ligo, .mligo, and .jsligo respectively). (alias: -s)

**-help**
print this help text and exit (alias: -?)


