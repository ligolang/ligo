compile parameters to a Michelson expression.

ligo compile parameter SOURCE_FILE PARAMETER_EXPRESSION

This sub-command compiles a parameter for a given contract to a
Michelson expression. The resulting Michelson expression can be passed
as an argument in a transaction which calls a contract.

=== flags ===

\[\--amount INT\] the tezos amount the Michelson interpreter will use
for the transaction. \[\--balance INT\] the balance the Michelson
interpreter will use for the contract balance. \[\--display-format
format\] the format that will be used by the CLI. Available formats are
\'dev\', \'json\', and \'human-readable\' (default). When human-readable
lacks details (we are still tweaking it), please contact us and use
another format in the meanwhile. (alias: \--format)
\[\--michelson-format format\] is the format that will be used by
compile-contract for the resulting Michelson. Available formats are
\'text\' (default), \'json\' and \'hex\'. \[\--no-warn\] disable warning
messages \[\--now TIMESTAMP\] the NOW value the Michelson interpreter
will use (e.g. \'2000-01-01T10:10:10Z\') \[\--output-file FILENAME\] if
used, prints the output into the specified file instead of stdout
(alias: -o) \[\--sender ADDRESS\] the sender the Michelson interpreter
transaction will use. \[\--source ADDRESS\] the source the Michelson
interpreter transaction will use. \[\--syntax SYNTAX\] the syntax that
will be used. Currently supported syntaxes are \"pascaligo\",
\"cameligo\", \"reasonligo\" and \"jsligo\". By default, the syntax is
guessed from the extension (.ligo, .mligo, .religo, and .jsligo
respectively). (alias: -s) \[\--werror\] treat warnings as errors \[-e
ENTRY-POINT\] the entry-point that will be compiled. (alias:
\--entry-point) \[-p PROTOCOL\] choose protocol\'s types/values
pre-loaded into the LIGO environment (edo , hangzhou). By default, the
current protocol (edo) will be used (alias: \--protocol) \[-help\] print
this help text and exit (alias: -?)
