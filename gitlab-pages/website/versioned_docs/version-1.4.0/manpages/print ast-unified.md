
### SYNOPSIS
```
ligo print ast-unified SOURCE_FILE
```

### DESCRIPTION
This sub-command prints the source file in the AST unified stage (with nanopasses).

### FLAGS
**--display-format FORMAT**
the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile. (alias: --format)

**--hide-sort restrict**
sorts shown in s-exp. available sorts: "ty_expr""pattern""instruction""statement""block""declaration""mod_expr""expr""program""program_entry" (alias: -hide)

**--library LIBS**
A comma-separated list of paths to directories where to search for files to be included by the preprocessor (alias: -l)

**--nanopass NANOPASS**
the nanopass name before/after which we stop executing the nanopasses. Use NAME+ for after and NAME for before, case do not matter (only for debug prints) (alias: -nano)

**--no-color**
disable coloring in CLI output

**--project-root PATH**
The path to root of the project.

**--show-loc**
show location in s-expressions

**--skip-analytics**
Avoid ligo analytics publication. Configurable with environment variable LIGO_SKIP_ANALYTICS too

**--syntax SYNTAX**
the syntax that will be used. Currently supported syntaxes are "cameligo" and "jsligo". By default, the syntax is guessed from the extension (.mligo and .jsligo respectively). (alias: -s)

**-help**
print this help text and exit (alias: -?)


