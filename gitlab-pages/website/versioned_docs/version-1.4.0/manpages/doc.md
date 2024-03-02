
### SYNOPSIS
```
ligo doc DIRECTORY
```

### DESCRIPTION
[BETA] Generate a documentation for your project.

### FLAGS
**--doc-args ARGUMENTS**
Arguments that would be passed into documentation generating tool (typedoc if --type-doc is enabled)

**--mdx**
[BETA

**--output-dir FILENAME**
if used, writes the output files into specified dir (alias: -o)

**--skip-analytics**
Avoid ligo analytics publication. Configurable with environment variable LIGO_SKIP_ANALYTICS too

**--syntax SYNTAX**
the syntax that will be used. Currently supported syntaxes are "cameligo" and "jsligo". By default, the syntax is guessed from the extension (.mligo and .jsligo respectively). (alias: -s)

**--type-doc**
Translate JsLIGO program into TypeScript for generating documentation.

**-p PROTOCOL**
choose protocol's types/values pre-loaded into the LIGO environment (oxford2). By default, the current protocol (oxford2) will be used (alias: --protocol)

**-help**
print this help text and exit (alias: -?)


