
### SYNOPSIS
```
ligo print signature SOURCE_FILE
```

### DESCRIPTION
This sub-command prints the source file signature.

### FLAGS
**--display-format FORMAT**
the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile. (alias: --format)

**--feature-infer-array-as-list**
Infer array as lists.

**--library LIBS**
A comma-separated list of paths to directories where to search for files to be included by the preprocessor (alias: -l)

**--no-color**
disable coloring in CLI output

**--project-root PATH**
The path to root of the project.

**--self-pass**
apply the self pass

**--skip-analytics**
Avoid ligo analytics publication. Configurable with environment variable LIGO_SKIP_ANALYTICS too

**--syntax SYNTAX**
the syntax that will be used. Currently supported syntaxes are "cameligo" and "jsligo". By default, the syntax is guessed from the extension (.mligo and .jsligo respectively). (alias: -s)

**--test**
force testing mode.

**--warn-infinite-loop**
warn about infinite loop

**--warn-unused-rec**
warn about unused recursion in a recursive function

**-m MODULE**
the entry-point will be compiled from that module. Files containing a single contract module are automatically infered (alias: --module)

**-p PROTOCOL**
choose protocol's types/values pre-loaded into the LIGO environment (parisb). By default, the current protocol (parisb) will be used (alias: --protocol)

**-help**
print this help text and exit (alias: -?)


