
### SYNOPSIS
```
ligo mutate cst SOURCE_FILE
```

### DESCRIPTION
This sub-command returns a mutated version for a given file. It does not use the build system.

### FLAGS
**--deprecated**
enable deprecated language PascaLIGO

**--display-format FORMAT**
the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile. (alias: --format)

**--library LIBS**
A comma-separated list of paths to directories where to search for files to be included by the preprocessor (alias: -l)

**--no-color**
disable coloring in CLI output

**--project-root PATH**
The path to root of the project.

**--seed SEED**
the seed or counter used for generation.

**--skip-analytics**
Avoid ligo analytics publication. Configurable with environment variable LIGO_SKIP_ANALYTICS too

**--syntax SYNTAX**
the syntax that will be used. Currently supported syntaxes are "cameligo" and "jsligo". By default, the syntax is guessed from the extension (.mligo and .jsligo respectively). (alias: -s)

**-generator GEN**
the generator for mutation. (alias: -g)

**-p PROTOCOL**
choose protocol's types/values pre-loaded into the LIGO environment (lima , mumbai). By default, the current protocol (mumbai) will be used (alias: --protocol)

**-help**
print this help text and exit (alias: -?)


