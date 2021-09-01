### NAME

ligo-pretty-print - Subcommand: Pretty-print the source file.

### SYNOPSIS

**ligo pretty-print** \[*OPTION*\]\... *SOURCE_FILE*

### DESCRIPTION

This sub-command pretty-prints a source file in LIGO. The width of the
pretty-printed text is adjusted to the number of columns in the terminal
(or 60 if it cannot be determined).

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
