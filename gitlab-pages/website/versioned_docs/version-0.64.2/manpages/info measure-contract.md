
### SYNOPSIS
```
ligo info measure-contract SOURCE_FILE
```

### DESCRIPTION
This sub-command compiles a source file and measures the contract's compiled size in bytes.

### FLAGS
**--deprecated**
enable deprecated language PascaLIGO

**--display-format FORMAT**
the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile. (alias: --format)

**--enable-michelson-typed-opt**
Enable Michelson optimizations that work using typecking.

**--no-color**
disable coloring in CLI output

**--no-warn**
disable warning messages

**--project-root PATH**
The path to root of the project.

**--skip-analytics**
Avoid ligo analytics publication. Configurable with environment variable LIGO_SKIP_ANALYTICS too

**--syntax SYNTAX**
the syntax that will be used. Currently supported syntaxes are "cameligo" and "jsligo". By default, the syntax is guessed from the extension (.mligo and .jsligo respectively). (alias: -s)

**--views VIEWS**
A list of declaration name that will be compiled as on-chain views, separated by ',' (alias: -v)

**--warn-unused-rec**
warn about unused recursion in a recursive function

**--werror**
treat warnings as errors

**-e ENTRY-POINT**
the entry-point that will be compiled. (alias: --entry-point)

**-p PROTOCOL**
choose protocol's types/values pre-loaded into the LIGO environment (lima , mumbai). By default, the current protocol (mumbai) will be used (alias: --protocol)

**-help**
print this help text and exit (alias: -?)


