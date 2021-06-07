### NAME

ligo-compile-expression - Subcommand: Compile to a Michelson value.

### SYNOPSIS

**ligo compile-expression** \[*OPTION*\]\... *SYNTAX* *\_EXPRESSION*

### DESCRIPTION

This sub-command compiles a LIGO expression to a Michelson value. It
works by compiling the LIGO expression to a Michelson expression and
then interpreting it using Michelson\`s interpreter.

### ARGUMENTS

*\_EXPRESSION* (required)

:   *\_EXPRESSION* is the expression that will be compiled.

*SYNTAX* (required)

:   *SYNTAX* is the syntax that will be used. Currently supported
    syntaxes are \"pascaligo\", \"cameligo\" and \"reasonligo\". By
    default, the syntax is guessed from the extension (.ligo, .mligo,
    .religo, .jsligo respectively).

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

**\--init-file**=*INIT_FILE*

:   *INIT_FILE* is the path to smart contract file to be used for
    context initialization.

**\--michelson-format**=*MICHELSON_FORMAT* (absent=text)

:   *MICHELSON_FORMAT* is the format that will be used by
    compile-contract for the resulting Michelson. Available formats are
    \`text\` (default), \`json\` and \`hex\`.

**-p** *PROTOCOL_VERSION*, **\--protocol**=*PROTOCOL_VERSION* (absent=current)

:   *PROTOCOL_VERSION* will decide protocol\`s types/values pre-loaded
    into the LIGO environment (edo). By default, the current protocol
    (edo) will be used

**\--version**

:   Show version information.

**\--warn**=*BOOL* (absent=true)

:   *BOOL* indicates whether warning messages should be printed in
    stderr or not

**\--werror**=*BOOL* (absent=false)

:   *BOOL* indicates whether warning messages should be treated as
    errors or not
