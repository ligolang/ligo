
### SYNOPSIS
```
ligo info dump-cst SOURCE_FILES [SOURCE_FILES ...]
```

### DESCRIPTION
This subcommand returns a concrete syntax tree for a LIGO contract. If format is not specified, then CST would be returned in the MessagePack format.

### FLAGS
**--display-format FORMAT**
the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile. (alias: --format)

**--library LIBS**
A comma-separated list of paths to directories where to search for files to be included by the preprocessor (alias: -l)

**--no-color**
disable coloring in CLI output

**--skip-analytics**
Avoid ligo analytics publication. Configurable with environment variable LIGO_SKIP_ANALYTICS too

**--with-types**
Tries to infer types for all named expressions

**-p PROTOCOL**
choose protocol's types/values pre-loaded into the LIGO environment (deprecated) (alias: --protocol)

**-help**
print this help text and exit (alias: -?)


