### NAME

ligo-print-graph - Subcommand: Print the dependency graph. Warning:
Intended for development of LIGO and can break at any time.

### SYNOPSIS

**ligo print-graph** \[*OPTION*\]\... *SOURCE_FILE*

### DESCRIPTION

This sub-command prints the dependency graph created by the module
system. It explores all imported source files (recursively) following a
DFS strategy.

### ARGUMENTS

*SOURCE_FILE* (required)

:   *SOURCE_FILE* is the path to the smart contract file.

### OPTIONS

**\--format**=*DISPLAY_FORMAT*, **\--display-format**=*DISPLAY_FORMAT* (absent=human-readable)

:   *DISPLAY_FORMAT* is the format that will be used by the CLI.
    Available formats are \`dev\`, \`json\`, and \`human-readable\`
    (default). When human-readable lacks details (we are still tweaking
    it), please contact us and use another format in the meanwhile.

**\--help**\[=*FMT*\] (default=auto)

:   Show this help in format *FMT*. The value *FMT* must be one of
    \`auto\`, \`pager\`, \`groff\` or \`plain\`. With \`auto\`, the
    format is \`pager\` or \`plain\` whenever the **TERM** env var is
    \`dumb\` or undefined.

**-s** *SYNTAX*, **\--syntax**=*SYNTAX* (absent=auto)

:   *SYNTAX* is the syntax that will be used. Currently supported
    syntaxes are \"pascaligo\", \"cameligo\", \"reasonligo\" and
    \"jsligo\". By default, the syntax is guessed from the extension
    (.ligo, .mligo, .religo, and .jsligo respectively).

**\--version**

:   Show version information.
