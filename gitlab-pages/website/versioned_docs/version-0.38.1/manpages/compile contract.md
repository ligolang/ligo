compile a contract.

ligo compile contract SOURCE_FILE

This sub-command compiles a contract to Michelson code. It expects a
source file and an entrypoint function that has the type of a contract:
\"parameter \* storage -\> operations list \* storage\".

=== flags ===

\[\--disable-michelson-typechecking\] Disable Michelson typecking, this
might produce ill-typed Michelson code. \[\--display-format format\] the
format that will be used by the CLI. Available formats are \'dev\',
\'json\', and \'human-readable\' (default). When human-readable lacks
details (we are still tweaking it), please contact us and use another
format in the meanwhile. (alias: \--format) \[\--michelson-comments
Selects\] \... kinds of comments to be added to the Michelson output.
Currently only \'location\' is supported, which propagates original
source locations (line/col). \[\--michelson-format format\] is the
format that will be used by compile-contract for the resulting
Michelson. Available formats are \'text\' (default), \'json\' and
\'hex\'. \[\--no-warn\] disable warning messages \[\--output-file
FILENAME\] if used, prints the output into the specified file instead of
stdout (alias: -o) \[\--project-root PATH\] The path to root of the
project. \[\--syntax SYNTAX\] the syntax that will be used. Currently
supported syntaxes are \"pascaligo\", \"cameligo\", \"reasonligo\" and
\"jsligo\". By default, the syntax is guessed from the extension (.ligo,
.mligo, .religo, and .jsligo respectively). (alias: -s) \[\--views
VIEWS\] A list of declaration name that will be compiled as on-chain
views, separated by \',\' (alias: -v) \[\--werror\] treat warnings as
errors \[-e ENTRY-POINT\] the entry-point that will be compiled. (alias:
\--entry-point) \[-p PROTOCOL\] choose protocol\'s types/values
pre-loaded into the LIGO environment (edo , hangzhou). By default, the
current protocol (edo) will be used (alias: \--protocol) \[-help\] print
this help text and exit (alias: -?)
