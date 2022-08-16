
### SYNOPSIS
ligo transpile expression SYNTAX _EXPRESSION SYNTAX

### DESCRIPTION
This sub-command transpiles a LIGO expression to another syntax. Comments are currently not transpiled. Please use at your own risk.

### FLAGS
**--display-format FORMAT**
the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile. (alias: --format)

**--pascaligo-dialect DIALECT**
the pascaligo dialect that will be used. Currently supported dialects are "terse" and "verbose". By default the dialect is "terse". (aliases: -d, -dialect)

**-help**
print this help text and exit (alias: -?)


