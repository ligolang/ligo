return the JSON encoded environment for a given file.

ligo info get-scope SOURCE_FILE

This sub-command returns the environment for a given file in JSON
format. It does not use the build system.

=== flags ===

\[\--display-format format\] the format that will be used by the CLI.
Available formats are \'dev\', \'json\', and \'human-readable\'
(default). When human-readable lacks details (we are still tweaking it),
please contact us and use another format in the meanwhile. (alias:
\--format) \[\--library LIBS\] \... A list of path to a directory
containing included files, separated by \',\' (alias: -l) \[\--syntax
SYNTAX\] the syntax that will be used. Currently supported syntaxes are
\"pascaligo\", \"cameligo\", \"reasonligo\" and \"jsligo\". By default,
the syntax is guessed from the extension (.ligo, .mligo, .religo, and
.jsligo respectively). (alias: -s) \[\--with-types\] Tries to infer
types for all named expressions \[-p PROTOCOL\] choose protocol\'s
types/values pre-loaded into the LIGO environment (edo , hangzhou). By
default, the current protocol (edo) will be used (alias: \--protocol)
\[-help\] print this help text and exit (alias: -?)
