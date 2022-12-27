
### SYNOPSIS
```
ligo print preprocessed SOURCE_FILE
```

### DESCRIPTION
This sub-command runs the pre-processor on a LIGO source file and outputs the result. The directive `#include` directly inlines the included file and therefore its content appears in the output. In contrast, the directive `#import` includes the file as a module and therefore the content of the imported file is not printed by this sub-command.

### FLAGS
**--display-format FORMAT**
the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile. (alias: --format)

**--library LIBS**
A comma-separated list of paths to directories where to search for files to be included by the preprocessor (alias: -l)

**--no-colour**
disable coloring in CLI output

**--project-root PATH**
The path to root of the project.

**--syntax SYNTAX**
the syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo" and "jsligo". By default, the syntax is guessed from the extension (.ligo, .mligo, and .jsligo respectively). (alias: -s)

**-help**
print this help text and exit (alias: -?)


