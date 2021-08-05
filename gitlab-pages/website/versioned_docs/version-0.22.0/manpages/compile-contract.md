### NAME

ligo-compile-contract - Subcommand: Compile a contract.

### SYNOPSIS

**ligo compile-contract** \[*OPTION*\]\... *SOURCE_FILE* *ENTRY_POINT*

### DESCRIPTION

This sub-command compiles a contract to Michelson code. It expects a
source file and an entrypoint function that has the type of a contract:
\"parameter \* storage -\> operations list \* storage\".

### ARGUMENTS

*ENTRY_POINT* (required)

:   *ENTRY_POINT* is entry-point that will be compiled.

*SOURCE_FILE* (required)

:   *SOURCE_FILE* is the path to the smart contract file.

### OPTIONS

**\--disable-michelson-typechecking**

:   disable Michelson typecking, this might produce ill-typed Michelson
    code.

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

**\--michelson-format**=*MICHELSON_FORMAT* (absent=text)

:   *MICHELSON_FORMAT* is the format that will be used by
    compile-contract for the resulting Michelson. Available formats are
    \`text\` (default), \`json\` and \`hex\`.

**\--output-file**=*OUTPUT_FILE*, **\--output**=*OUTPUT_FILE*

:   *OUTPUT_FILE* if used, prints the output into the specified file
    instead of stdout

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
