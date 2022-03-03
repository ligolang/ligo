print the typed AST. Warning: Intended for development of LIGO and can
break at any time.

ligo print ast-typed SOURCE_FILE

This sub-command prints the source file in the AST typed stage.
Internally, it uses the build system to type the contract, but the
contract is not combined with imported modules.

=== flags ===

\[\--display-format format\] the format that will be used by the CLI.
Available formats are \'dev\', \'json\', and \'human-readable\'
(default). When human-readable lacks details (we are still tweaking it),
please contact us and use another format in the meanwhile. (alias:
\--format) \[\--project-root PATH\] The path to root of the project.
\[\--self-pass\] apply the self pass \[\--syntax SYNTAX\] the syntax
that will be used. Currently supported syntaxes are \"pascaligo\",
\"cameligo\", \"reasonligo\" and \"jsligo\". By default, the syntax is
guessed from the extension (.ligo, .mligo, .religo, and .jsligo
respectively). (alias: -s) \[-p PROTOCOL\] choose protocol\'s
types/values pre-loaded into the LIGO environment (edo , hangzhou). By
default, the current protocol (edo) will be used (alias: \--protocol)
\[-help\] print this help text and exit (alias: -?)
