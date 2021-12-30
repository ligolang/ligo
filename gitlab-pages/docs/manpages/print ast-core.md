print the core ligo AST. Warning: Intended for development of LIGO and
can break at any time.

ligo print ast-core SOURCE_FILE

This sub-command prints the source file in the AST core stage.

=== flags ===

\[\--display-format format\] the format that will be used by the CLI.
Available formats are \'dev\', \'json\', and \'human-readable\'
(default). When human-readable lacks details (we are still tweaking it),
please contact us and use another format in the meanwhile. (alias:
\--format) \[\--syntax SYNTAX\] the syntax that will be used. Currently
supported syntaxes are \"pascaligo\", \"cameligo\", \"reasonligo\" and
\"jsligo\". By default, the syntax is guessed from the extension (.ligo,
.mligo, .religo, and .jsligo respectively). (alias: -s) \[-help\] print
this help text and exit (alias: -?)
