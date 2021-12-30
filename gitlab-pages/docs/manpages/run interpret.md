interpret the expression in the context initialized by the provided
source file.

ligo run interpret EXPRESSION_EXPRESSION

This sub-command interprets a LIGO expression. The context can be
initialized by providing a source file. The interpretation is done using
Michelson\'s interpreter.

=== flags ===

\[\--amount INT\] the tezos amount the Michelson interpreter will use
for the transaction. \[\--balance INT\] the balance the Michelson
interpreter will use for the contract balance. \[\--display-format
format\] the format that will be used by the CLI. Available formats are
\'dev\', \'json\', and \'human-readable\' (default). When human-readable
lacks details (we are still tweaking it), please contact us and use
another format in the meanwhile. (alias: \--format) \[\--init-file
FILENAME\] the path to the smart contract file to be used for context
initialization. \[\--now TIMESTAMP\] the NOW value the Michelson
interpreter will use (e.g. \'2000-01-01T10:10:10Z\') \[\--sender
ADDRESS\] the sender the Michelson interpreter transaction will use.
\[\--source ADDRESS\] the source the Michelson interpreter transaction
will use. \[\--syntax SYNTAX\] the syntax that will be used. Currently
supported syntaxes are \"pascaligo\", \"cameligo\", \"reasonligo\" and
\"jsligo\". By default, the syntax is guessed from the extension (.ligo,
.mligo, .religo, and .jsligo respectively). (alias: -s) \[-p PROTOCOL\]
choose protocol\'s types/values pre-loaded into the LIGO environment
(edo , hangzhou). By default, the current protocol (edo) will be used
(alias: \--protocol) \[-help\] print this help text and exit (alias: -?)
