### NAME

ligo-evaluate-call - Subcommand: Run a function with the given
parameter.

### SYNOPSIS

**ligo evaluate-call** \[*OPTION*\]\... *SOURCE_FILE* *ENTRY_POINT*
*PARAMETER_EXPRESSION*

### DESCRIPTION

This sub-command runs a LIGO function on a given argument. The context
is initialized from a source file where the function is implemented. The
interpretation is done using Michelson\`s interpreter.

### ARGUMENTS

*ENTRY_POINT* (required)

:   *ENTRY_POINT* is entry-point that will be compiled.

*PARAMETER_EXPRESSION* (required)

:   *PARAMETER_EXPRESSION* is the expression that will be compiled.

*SOURCE_FILE* (required)

:   *SOURCE_FILE* is the path to the smart contract file.

### OPTIONS

**\--amount**=*AMOUNT* (absent=0)

:   *AMOUNT* is the amount the Michelson interpreter will use for the
    transaction.

**\--balance**=*BALANCE* (absent=0)

:   *BALANCE* is the balance the Michelson interpreter will use for the
    contract balance.

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

**\--now**=*NOW*

:   *NOW* is the NOW value the Michelson interpreter will use (e.g.
    \`2000-01-01T10:10:10Z\`)

**-p** *PROTOCOL_VERSION*, **\--protocol**=*PROTOCOL_VERSION* (absent=current)

:   *PROTOCOL_VERSION* will decide protocol\`s types/values pre-loaded
    into the LIGO environment (edo). By default, the current protocol
    (edo) will be used

**-s** *SYNTAX*, **\--syntax**=*SYNTAX* (absent=auto)

:   *SYNTAX* is the syntax that will be used. Currently supported
    syntaxes are \"pascaligo\", \"cameligo\", \"reasonligo\" and
    \"jsligo\". By default, the syntax is guessed from the extension
    (.ligo, .mligo, .religo, and .jsligo respectively).

**\--sender**=*SENDER*

:   *SENDER* is the sender the Michelson interpreter transaction will
    use.

**\--source**=*SOURCE*

:   *SOURCE* is the source the Michelson interpreter transaction will
    use.

**\--version**

:   Show version information.

**\--warn**=*BOOL* (absent=true)

:   *BOOL* indicates whether warning messages should be printed in
    stderr or not

**\--werror**=*BOOL* (absent=false)

:   *BOOL* indicates whether warning messages should be treated as
    errors or not
