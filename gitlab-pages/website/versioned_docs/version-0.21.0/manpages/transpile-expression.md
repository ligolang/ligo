### NAME

ligo-transpile-expression - Subcommand: Transpile an expression to
another syntax (BETA).

### SYNOPSIS

**ligo transpile-expression** \[*OPTION*\]\... *SYNTAX* *\_EXPRESSION*
*SYNTAX*

### DESCRIPTION

This sub-command transpiles a LIGO expression to another syntax.
Comments are currently not transpiled. Please use at your own risk.

### ARGUMENTS

*\_EXPRESSION* (required)

:   *\_EXPRESSION* is the expression that will be compiled.

*SYNTAX* (required)

:   *SYNTAX* is the syntax that will be used. Currently supported
    syntaxes are \"pascaligo\", \"cameligo\" and \"reasonligo\". By
    default, the syntax is guessed from the extension (.ligo, .mligo,
    .religo, .jsligo respectively).

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

**\--version**

:   Show version information.
