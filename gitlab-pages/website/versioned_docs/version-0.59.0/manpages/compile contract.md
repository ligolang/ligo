
### SYNOPSIS
```
ligo compile contract SOURCE_FILE
```

### DESCRIPTION
This sub-command compiles a contract to Michelson code. It expects a source file and an entrypoint function that has the type of a contract: "parameter * storage -> operations list * storage".

### FLAGS
**--constants CONSTANTS**
A list of global constants that will be assumed in the context, separated by ',' (alias: -c)

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

**--michelson-comments COMMENT_TYPE**
.. Selects kinds of comments to be added to the Michelson output. Currently 'location' and 'env' are supported. 'location' propagates original source locations. 'env' inserts additional empty Seq nodes with comments relating the Michelson stack to the source LIGO environment.

**--michelson-format CODE_FORMAT**
format that will be used by compile-contract for the resulting Michelson. Available formats are 'text' (default), 'json' and 'hex'.

**--no-colour**
disable coloring in CLI output

**--no-stdlib**
disable stdlib inclusion.

**--no-warn**
disable warning messages

**--output-file FILENAME**
if used, prints the output into the specified file instead of stdout (alias: -o)

**--project-root PATH**
The path to root of the project.

**--syntax SYNTAX**
the syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo" and "jsligo". By default, the syntax is guessed from the extension (.ligo, .mligo, and .jsligo respectively). (alias: -s)

**--views VIEWS**
A list of declaration name that will be compiled as on-chain views, separated by ',' (alias: -v)

**--warn-unused-rec**
warn about unused recursion in a recursive function

**--werror**
treat warnings as errors

**-e ENTRY-POINT**
the entry-point that will be compiled. (alias: --entry-point)

**-p PROTOCOL**
choose protocol's types/values pre-loaded into the LIGO environment (kathmandu , lima). By default, the current protocol (lima) will be used (alias: --protocol)

**-help**
print this help text and exit (alias: -?)


