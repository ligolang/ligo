
### SYNOPSIS
```
ligo compile view SOURCE_FILE VIEW
```

### DESCRIPTION
This sub-command compiles a view to Michelson code. It expects a source file and a view function that has the type of a view: "parameter * storage -> result".

### FLAGS
**--constants CONSTANTS**
A list of global constants that will be assumed in the context, separated by ',' (alias: -c)

**--deprecated**
enable deprecated language PascaLIGO

**--disable-michelson-typechecking**
Disable Michelson typecking, this might produce ill-typed Michelson code.

**--display-format FORMAT**
the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile. (alias: --format)

**--enable-michelson-typed-opt**
Enable Michelson optimizations that work using typecking.

**--experimental-disable-optimizations-for-debugging**
Experimental: Disable certain optimizations in order to simplify the relationship between the source LIGO and the target Michelson. Intended for use with stepwise Michelson debuggers.

**--file-constants FILE_CONSTANTS**
A file with a JSON list of strings with Michelson code. Those Michelson values will be registered as global constants in the context.

**--library LIBS**
A comma-separated list of paths to directories where to search for files to be included by the preprocessor (alias: -l)

**--michelson-format CODE_FORMAT**
format that will be used by compile-contract for the resulting Michelson. Available formats are 'text' (default), 'json' and 'hex'.

**--no-color**
disable coloring in CLI output

**--no-metadata-check**
disable TZIP-16 metadata compliance check

**--no-stdlib**
disable stdlib inclusion.

**--no-warn**
disable warning messages

**--output-file FILENAME**
if used, prints the output into the specified file instead of stdout (alias: -o)

**--project-root PATH**
The path to root of the project.

**--skip-analytics**
Avoid ligo analytics publication. Configurable with environment variable LIGO_SKIP_ANALYTICS too

**--syntax SYNTAX**
the syntax that will be used. Currently supported syntaxes are "cameligo" and "jsligo". By default, the syntax is guessed from the extension (.mligo and .jsligo respectively). (alias: -s)

**--transpiled**
Disable checks that are unapplicable to transpiled contracts.

**--warn-infinite-loop**
warn about infinite loop

**--warn-unused-rec**
warn about unused recursion in a recursive function

**--werror**
treat warnings as errors

**-e ENTRY-POINT**
the entry-point that will be compiled. (alias: --entry-point)

**-m MODULE**
the entry-point will be compiled from that module. (alias: --module)

**-p PROTOCOL**
choose protocol's types/values pre-loaded into the LIGO environment (mumbai , nairobi). By default, the current protocol (nairobi) will be used (alias: --protocol)

**-help**
print this help text and exit (alias: -?)


