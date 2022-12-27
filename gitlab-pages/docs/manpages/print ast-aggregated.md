
### SYNOPSIS
```
ligo print ast-aggregated SOURCE_FILE
```

### DESCRIPTION
This sub-command prints the source file in the AST aggregated stage.

### FLAGS
**--display-format FORMAT**
the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile. (alias: --format)

**--no-colour**
disable coloring in CLI output

**--project-root PATH**
The path to root of the project.

**--self-pass**
apply the self pass

**--syntax SYNTAX**
the syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo" and "jsligo". By default, the syntax is guessed from the extension (.ligo, .mligo, and .jsligo respectively). (alias: -s)

**--test**
force testing mode.

**--warn-unused-rec**
warn about unused recursion in a recursive function

**-p PROTOCOL**
choose protocol's types/values pre-loaded into the LIGO environment (kathmandu , lima). By default, the current protocol (lima) will be used (alias: --protocol)

**-help**
print this help text and exit (alias: -?)


