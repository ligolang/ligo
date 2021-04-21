### NAME

ligo-measure-contract - Subcommand: Measure a contract\`s compiled size
in bytes.

### SYNOPSIS

**ligo measure-contract** \[*OPTION*\]\... *SOURCE_FILE* *ENTRY_POINT*

### DESCRIPTION

This sub-command compiles a source file and measures the contract\`s
compiled size in bytes.

### ARGUMENTS

*ENTRY_POINT* (required)

:   *ENTRY_POINT* is entry-point that will be compiled.

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

**\--infer**

:   enable type inference

**-p** *PROTOCOL_VERSION*, **\--protocol**=*PROTOCOL_VERSION* (absent=current)

:   *PROTOCOL_VERSION* will decide protocol\`s types/values pre-loaded
    into the LIGO environment (edo). By default, the current protocol
    (edo) will be used

**-s** *SYNTAX*, **\--syntax**=*SYNTAX* (absent=auto)

:   *SYNTAX* is the syntax that will be used. Currently supported
    syntaxes are \"pascaligo\", \"cameligo\", \"reasonligo\" and
    \"jsligo\". By default, the syntax is guessed from the extension
    (.ligo, .mligo, .religo, and .jsligo respectively).

**\--version**

:   Show version information.

**\--warn**=*BOOL* (absent=true)

:   *BOOL* indicates whether warning messages should be printed in
    stderr or not

**\--werror**=*BOOL* (absent=false)

:   *BOOL* indicates whether warning messages should be treated as
    errors or not
