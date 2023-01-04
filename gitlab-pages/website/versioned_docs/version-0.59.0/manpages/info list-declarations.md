
### SYNOPSIS
```
ligo info list-declarations SOURCE_FILE
```

### DESCRIPTION
This sub-command prints a list of all top-level declarations (not including types and modules).

### FLAGS
**--display-format FORMAT**
the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile. (alias: --format)

**--only-ep**
Only display declarations that have the type of an entrypoint

**--project-root PATH**
The path to root of the project.

**--syntax SYNTAX**
the syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo" and "jsligo". By default, the syntax is guessed from the extension (.ligo, .mligo, and .jsligo respectively). (alias: -s)

**-help**
print this help text and exit (alias: -?)


