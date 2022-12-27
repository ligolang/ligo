
### SYNOPSIS
```
ligo mutate cst SOURCE_FILE
```

### DESCRIPTION
This sub-command returns a mutated version for a given file. It does not use the build system.

### FLAGS
**--display-format FORMAT**
the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile. (alias: --format)

**--library LIBS**
A comma-separated list of paths to directories where to search for files to be included by the preprocessor (alias: -l)

**--project-root PATH**
The path to root of the project.

**--seed SEED**
the seed or counter used for generation.

**--syntax SYNTAX**
the syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo" and "jsligo". By default, the syntax is guessed from the extension (.ligo, .mligo, and .jsligo respectively). (alias: -s)

**-generator GEN**
the generator for mutation. (alias: -g)

**-p PROTOCOL**
choose protocol's types/values pre-loaded into the LIGO environment (kathmandu , lima). By default, the current protocol (lima) will be used (alias: --protocol)

**-help**
print this help text and exit (alias: -?)


