compile to a Michelson value.

ligo compile expression SYNTAX \_EXPRESSION

This sub-command compiles a LIGO expression to a Michelson value. It
works by compiling the LIGO expression to a Michelson expression and
then interpreting it using Michelson\'s interpreter.

=== flags ===

\[\--display-format format\] the format that will be used by the CLI.
Available formats are \'dev\', \'json\', and \'human-readable\'
(default). When human-readable lacks details (we are still tweaking it),
please contact us and use another format in the meanwhile. (alias:
\--format) \[\--init-file FILENAME\] the path to the smart contract file
to be used for context initialization. \[\--michelson-format format\] is
the format that will be used by compile-contract for the resulting
Michelson. Available formats are \'text\' (default), \'json\' and
\'hex\'. \[\--no-warn\] disable warning messages \[\--project-root
PATH\] The path to root of the project. \[\--werror\] treat warnings as
errors \[\--without-run\] disable running of compiled expression. \[-p
PROTOCOL\] choose protocol\'s types/values pre-loaded into the LIGO
environment (edo , hangzhou). By default, the current protocol (edo)
will be used (alias: \--protocol) \[-help\] print this help text and
exit (alias: -?)
