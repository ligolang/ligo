
### SYNOPSIS
```
ligo info get-scope SOURCE_FILE
```

### DESCRIPTION
This sub-command returns the environment for a given file in JSON format. It does not use the build system.

### FLAGS
**--defs-only**
Gets only list of definitions (without scopes).

**--display-format FORMAT**
the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile. (alias: --format)

**--library LIBS**
A comma-separated list of paths to directories where to search for files to be included by the preprocessor (alias: -l)

**--no-color**
disable coloring in CLI output

**--no-stdlib**
disable stdlib inclusion.

**--project-root PATH**
The path to root of the project.

**--with-types**
Tries to infer types for all named expressions

**-p PROTOCOL**
choose protocol's types/values pre-loaded into the LIGO environment (mumbai , nairobi). By default, the current protocol (nairobi) will be used (alias: --protocol)

**-help**
print this help text and exit (alias: -?)


