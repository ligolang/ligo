
### SYNOPSIS
```
ligo init contract [PROJECT_NAME]
```

### DESCRIPTION
Generate new folder from contract template. Internet connexion needed

### FLAGS
**--display-format FORMAT**
the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile. (alias: --format)

**--no-color**
disable coloring in CLI output

**--registry URL**
The url to a LIGO registry.

**--skip-analytics**
Avoid ligo analytics publication. Configurable with environment variable LIGO_SKIP_ANALYTICS too

**--template TEMPLATE**
the template name which will be used to generate folder. You can obtain available list by running ligo init list. If not provided default is empty-project. (alias: -t)

**--template-list**
If present, change cmmand behavior and list available templates for this command.

**-help**
print this help text and exit (alias: -?)


