### NAME

ligo-transpile-contract - Subcommand: Transpile a contract to another
syntax (BETA).

### SYNOPSIS

**ligo transpile-contract** \[*OPTION*\]\... *SOURCE_FILE* *SYNTAX*

### DESCRIPTION

This sub-command transpiles a source file to another syntax. It does not
use the build system, but the source file is preprocessed. Comments are
currently not transpiled. Please use at your own risk.

### ARGUMENTS

*SOURCE_FILE* (required)

:   *SOURCE_FILE* is the path to the smart contract file.

*SYNTAX* (required)

:   *SYNTAX* is the syntax that will be used. Currently supported
    syntaxes are \"pascaligo\", \"cameligo\" and \"reasonligo\". By
    default, the syntax is guessed from the extension (.ligo, .mligo,
    .religo, .jsligo respectively).

### OPTIONS

**-d** *PASCALIGO_DIALECT*, **\--dialect**=*PASCALIGO_DIALECT* (absent=terse)

:   *PASCALIGO_DIALECT* is the pascaligo dialect that will be used.
    Currently supported dialects are \"terse\" and \"verbose\". By
    default the dialect is \"terse\".

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

**\--output-file**=*OUTPUT_FILE*, **\--output**=*OUTPUT_FILE*

:   *OUTPUT_FILE* if used, prints the output into the specified file
    instead of stdout

**-s** *SYNTAX*, **\--syntax**=*SYNTAX* (absent=auto)

:   *SYNTAX* is the syntax that will be used. Currently supported
    syntaxes are \"pascaligo\", \"cameligo\", \"reasonligo\" and
    \"jsligo\". By default, the syntax is guessed from the extension
    (.ligo, .mligo, .religo, and .jsligo respectively).

**\--version**

:   Show version information.
