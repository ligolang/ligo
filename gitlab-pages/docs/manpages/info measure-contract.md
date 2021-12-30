measure a contract\'s compiled size in bytes.

ligo info measure-contract SOURCE_FILE

This sub-command compiles a source file and measures the contract\'s
compiled size in bytes.

=== flags ===

\[\--display-format format\] the format that will be used by the CLI.
Available formats are \'dev\', \'json\', and \'human-readable\'
(default). When human-readable lacks details (we are still tweaking it),
please contact us and use another format in the meanwhile. (alias:
\--format) \[\--no-warn\] disable warning messages \[\--syntax SYNTAX\]
the syntax that will be used. Currently supported syntaxes are
\"pascaligo\", \"cameligo\", \"reasonligo\" and \"jsligo\". By default,
the syntax is guessed from the extension (.ligo, .mligo, .religo, and
.jsligo respectively). (alias: -s) \[\--views VIEWS\] A list of
declaration name that will be compiled as on-chain views, separated by
\',\' (alias: -v) \[\--werror\] treat warnings as errors \[-e
ENTRY-POINT\] the entry-point that will be compiled. (alias:
\--entry-point) \[-p PROTOCOL\] choose protocol\'s types/values
pre-loaded into the LIGO environment (edo , hangzhou). By default, the
current protocol (edo) will be used (alias: \--protocol) \[-help\] print
this help text and exit (alias: -?)
