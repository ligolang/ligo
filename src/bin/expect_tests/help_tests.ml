open Cli_expect

let%expect_test _ =
  (* TODO good? *)
  run_ligo_good [] ;
  [%expect {|
    NAME
           ligo

    SYNOPSIS
           ligo COMMAND ...

    COMMANDS
           compile-contract
               Subcommand: compile a contract. See `ligo compile-contract --help'
               for a list of options specific to this subcommand.

           compile-expression
               Subcommand: compile to a michelson value.

           compile-parameter
               Subcommand: compile parameters to a michelson expression. The
               resulting michelson expression can be passed as an argument in a
               transaction which calls a contract. See `ligo compile-parameter
               --help' for a list of options specific to this subcommand.

           compile-storage
               Subcommand: compile an initial storage in ligo syntax to a
               michelson expression. The resulting michelson expression can be
               passed as an argument in a transaction which originates a
               contract. See `ligo compile-storage --help' for a list of options
               specific to this subcommand.

           dry-run
               Subcommand: run a smart-contract with the given storage and input.

           evaluate-value
               Subcommand: evaluate a given definition.

           run-function
               Subcommand: run a function with the given parameter.

    OPTIONS
           --help[=FMT] (default=auto)
               Show this help in format FMT. The value FMT must be one of `auto',
               `pager', `groff' or `plain'. With `auto', the format is `pager` or
               `plain' whenever the TERM env var is `dumb' or undefined. |} ] ;

  run_ligo_good [ "--help" ] ;
  [%expect {|
    NAME
           ligo

    SYNOPSIS
           ligo COMMAND ...

    COMMANDS
           compile-contract
               Subcommand: compile a contract. See `ligo compile-contract --help'
               for a list of options specific to this subcommand.

           compile-expression
               Subcommand: compile to a michelson value.

           compile-parameter
               Subcommand: compile parameters to a michelson expression. The
               resulting michelson expression can be passed as an argument in a
               transaction which calls a contract. See `ligo compile-parameter
               --help' for a list of options specific to this subcommand.

           compile-storage
               Subcommand: compile an initial storage in ligo syntax to a
               michelson expression. The resulting michelson expression can be
               passed as an argument in a transaction which originates a
               contract. See `ligo compile-storage --help' for a list of options
               specific to this subcommand.

           dry-run
               Subcommand: run a smart-contract with the given storage and input.

           evaluate-value
               Subcommand: evaluate a given definition.

           run-function
               Subcommand: run a function with the given parameter.

    OPTIONS
           --help[=FMT] (default=auto)
               Show this help in format FMT. The value FMT must be one of `auto',
               `pager', `groff' or `plain'. With `auto', the format is `pager` or
               `plain' whenever the TERM env var is `dumb' or undefined. |} ] ;

  run_ligo_good [ "compile-contract" ; "--help" ] ;
  [%expect {|
    NAME
           ligo-compile-contract - Subcommand: compile a contract. See `ligo
           compile-contract --help' for a list of options specific to this
           subcommand.

    SYNOPSIS
           ligo compile-contract [OPTION]... SOURCE_FILE ENTRY_POINT

    ARGUMENTS
           ENTRY_POINT (required)
               ENTRY_POINT is entry-point that will be compiled.

           SOURCE_FILE (required)
               SOURCE_FILE is the path to the .ligo or .mligo file of the
               contract.

    OPTIONS
           --format=DISPLAY_FORMAT, --display-format=DISPLAY_FORMAT
           (absent=human-readable)
               DISPLAY_FORMAT is the format that will be used by the CLI.
               Available formats are 'dev', 'json', and 'human-readable'
               (default). When human-readable lacks details (we are still
               tweaking it), please contact us and use another format in the
               meanwhile.

           --help[=FMT] (default=auto)
               Show this help in format FMT. The value FMT must be one of `auto',
               `pager', `groff' or `plain'. With `auto', the format is `pager` or
               `plain' whenever the TERM env var is `dumb' or undefined.

           --michelson-format=MICHELSON_FORMAT (absent=text)
               MICHELSON_FORMAT is the format that will be used by
               compile-contract for the resulting Michelson. Available formats
               are 'text' (default), 'json' and 'hex'.

           -s SYNTAX, --syntax=SYNTAX (absent=auto)
               SYNTAX is the syntax that will be used. Currently supported
               syntaxes are "pascaligo" and "cameligo". By default, the syntax is
               guessed from the extension (.ligo and .mligo, respectively). |} ] ;

  run_ligo_good [ "compile-parameter" ; "--help" ] ;
  [%expect {|
    NAME
           ligo-compile-parameter - Subcommand: compile parameters to a michelson
           expression. The resulting michelson expression can be passed as an
           argument in a transaction which calls a contract. See `ligo
           compile-parameter --help' for a list of options specific to this
           subcommand.

    SYNOPSIS
           ligo compile-parameter [OPTION]... SOURCE_FILE ENTRY_POINT
           PARAMETER_EXPRESSION

    ARGUMENTS
           ENTRY_POINT (required)
               ENTRY_POINT is entry-point that will be compiled.

           PARAMETER_EXPRESSION (required)
               PARAMETER_EXPRESSION is the expression that will be compiled.

           SOURCE_FILE (required)
               SOURCE_FILE is the path to the .ligo or .mligo file of the
               contract.

    OPTIONS
           --format=DISPLAY_FORMAT, --display-format=DISPLAY_FORMAT
           (absent=human-readable)
               DISPLAY_FORMAT is the format that will be used by the CLI.
               Available formats are 'dev', 'json', and 'human-readable'
               (default). When human-readable lacks details (we are still
               tweaking it), please contact us and use another format in the
               meanwhile.

           --help[=FMT] (default=auto)
               Show this help in format FMT. The value FMT must be one of `auto',
               `pager', `groff' or `plain'. With `auto', the format is `pager` or
               `plain' whenever the TERM env var is `dumb' or undefined.

           --michelson-format=MICHELSON_FORMAT (absent=text)
               MICHELSON_FORMAT is the format that will be used by
               compile-contract for the resulting Michelson. Available formats
               are 'text' (default), 'json' and 'hex'.

           -s SYNTAX, --syntax=SYNTAX (absent=auto)
               SYNTAX is the syntax that will be used. Currently supported
               syntaxes are "pascaligo" and "cameligo". By default, the syntax is
               guessed from the extension (.ligo and .mligo, respectively). |} ] ;

  run_ligo_good [ "compile-storage" ; "--help" ] ;
  [%expect {|
    NAME
           ligo-compile-storage - Subcommand: compile an initial storage in ligo
           syntax to a michelson expression. The resulting michelson expression
           can be passed as an argument in a transaction which originates a
           contract. See `ligo compile-storage --help' for a list of options
           specific to this subcommand.

    SYNOPSIS
           ligo compile-storage [OPTION]... SOURCE_FILE ENTRY_POINT
           STORAGE_EXPRESSION

    ARGUMENTS
           ENTRY_POINT (required)
               ENTRY_POINT is entry-point that will be compiled.

           SOURCE_FILE (required)
               SOURCE_FILE is the path to the .ligo or .mligo file of the
               contract.

           STORAGE_EXPRESSION (required)
               STORAGE_EXPRESSION is the expression that will be compiled.

    OPTIONS
           --format=DISPLAY_FORMAT, --display-format=DISPLAY_FORMAT
           (absent=human-readable)
               DISPLAY_FORMAT is the format that will be used by the CLI.
               Available formats are 'dev', 'json', and 'human-readable'
               (default). When human-readable lacks details (we are still
               tweaking it), please contact us and use another format in the
               meanwhile.

           --help[=FMT] (default=auto)
               Show this help in format FMT. The value FMT must be one of `auto',
               `pager', `groff' or `plain'. With `auto', the format is `pager` or
               `plain' whenever the TERM env var is `dumb' or undefined.

           --michelson-format=MICHELSON_FORMAT (absent=text)
               MICHELSON_FORMAT is the format that will be used by
               compile-contract for the resulting Michelson. Available formats
               are 'text' (default), 'json' and 'hex'.

           -s SYNTAX, --syntax=SYNTAX (absent=auto)
               SYNTAX is the syntax that will be used. Currently supported
               syntaxes are "pascaligo" and "cameligo". By default, the syntax is
               guessed from the extension (.ligo and .mligo, respectively). |} ] ;

  run_ligo_good [ "dry-run" ; "--help" ] ;
  [%expect {|
    NAME
           ligo-dry-run - Subcommand: run a smart-contract with the given storage
           and input.

    SYNOPSIS
           ligo dry-run [OPTION]... SOURCE_FILE ENTRY_POINT PARAMETER_EXPRESSION
           STORAGE_EXPRESSION

    ARGUMENTS
           ENTRY_POINT (required)
               ENTRY_POINT is entry-point that will be compiled.

           PARAMETER_EXPRESSION (required)
               PARAMETER_EXPRESSION is the expression that will be compiled.

           SOURCE_FILE (required)
               SOURCE_FILE is the path to the .ligo or .mligo file of the
               contract.

           STORAGE_EXPRESSION (required)
               STORAGE_EXPRESSION is the expression that will be compiled.

    OPTIONS
           --amount=AMOUNT (absent=0)
               AMOUNT is the amount the dry-run transaction will use.

           --format=DISPLAY_FORMAT, --display-format=DISPLAY_FORMAT
           (absent=human-readable)
               DISPLAY_FORMAT is the format that will be used by the CLI.
               Available formats are 'dev', 'json', and 'human-readable'
               (default). When human-readable lacks details (we are still
               tweaking it), please contact us and use another format in the
               meanwhile.

           --help[=FMT] (default=auto)
               Show this help in format FMT. The value FMT must be one of `auto',
               `pager', `groff' or `plain'. With `auto', the format is `pager` or
               `plain' whenever the TERM env var is `dumb' or undefined.

           -s SYNTAX, --syntax=SYNTAX (absent=auto)
               SYNTAX is the syntax that will be used. Currently supported
               syntaxes are "pascaligo" and "cameligo". By default, the syntax is
               guessed from the extension (.ligo and .mligo, respectively).

           --sender=SENDER
               SENDER is the sender the dry-run transaction will use.

           --source=SOURCE
               SOURCE is the source the dry-run transaction will use. |} ] ;

  run_ligo_good [ "run-function" ; "--help" ] ;
  [%expect {|
    NAME
           ligo-run-function - Subcommand: run a function with the given
           parameter.

    SYNOPSIS
           ligo run-function [OPTION]... SOURCE_FILE ENTRY_POINT
           PARAMETER_EXPRESSION

    ARGUMENTS
           ENTRY_POINT (required)
               ENTRY_POINT is entry-point that will be compiled.

           PARAMETER_EXPRESSION (required)
               PARAMETER_EXPRESSION is the expression that will be compiled.

           SOURCE_FILE (required)
               SOURCE_FILE is the path to the .ligo or .mligo file of the
               contract.

    OPTIONS
           --amount=AMOUNT (absent=0)
               AMOUNT is the amount the dry-run transaction will use.

           --format=DISPLAY_FORMAT, --display-format=DISPLAY_FORMAT
           (absent=human-readable)
               DISPLAY_FORMAT is the format that will be used by the CLI.
               Available formats are 'dev', 'json', and 'human-readable'
               (default). When human-readable lacks details (we are still
               tweaking it), please contact us and use another format in the
               meanwhile.

           --help[=FMT] (default=auto)
               Show this help in format FMT. The value FMT must be one of `auto',
               `pager', `groff' or `plain'. With `auto', the format is `pager` or
               `plain' whenever the TERM env var is `dumb' or undefined.

           -s SYNTAX, --syntax=SYNTAX (absent=auto)
               SYNTAX is the syntax that will be used. Currently supported
               syntaxes are "pascaligo" and "cameligo". By default, the syntax is
               guessed from the extension (.ligo and .mligo, respectively).

           --sender=SENDER
               SENDER is the sender the dry-run transaction will use.

           --source=SOURCE
               SOURCE is the source the dry-run transaction will use. |} ] ;

  run_ligo_good [ "evaluate-value" ; "--help" ] ;
  [%expect {|
    NAME
           ligo-evaluate-value - Subcommand: evaluate a given definition.

    SYNOPSIS
           ligo evaluate-value [OPTION]... SOURCE_FILE ENTRY_POINT

    ARGUMENTS
           ENTRY_POINT (required)
               ENTRY_POINT is entry-point that will be compiled.

           SOURCE_FILE (required)
               SOURCE_FILE is the path to the .ligo or .mligo file of the
               contract.

    OPTIONS
           --amount=AMOUNT (absent=0)
               AMOUNT is the amount the dry-run transaction will use.

           --format=DISPLAY_FORMAT, --display-format=DISPLAY_FORMAT
           (absent=human-readable)
               DISPLAY_FORMAT is the format that will be used by the CLI.
               Available formats are 'dev', 'json', and 'human-readable'
               (default). When human-readable lacks details (we are still
               tweaking it), please contact us and use another format in the
               meanwhile.

           --help[=FMT] (default=auto)
               Show this help in format FMT. The value FMT must be one of `auto',
               `pager', `groff' or `plain'. With `auto', the format is `pager` or
               `plain' whenever the TERM env var is `dumb' or undefined.

           -s SYNTAX, --syntax=SYNTAX (absent=auto)
               SYNTAX is the syntax that will be used. Currently supported
               syntaxes are "pascaligo" and "cameligo". By default, the syntax is
               guessed from the extension (.ligo and .mligo, respectively).

           --sender=SENDER
               SENDER is the sender the dry-run transaction will use.

           --source=SOURCE
               SOURCE is the source the dry-run transaction will use. |} ] ;

  run_ligo_good [ "compile-expression" ; "--help" ] ;
  [%expect {|
    NAME
           ligo-compile-expression - Subcommand: compile to a michelson value.

    SYNOPSIS
           ligo compile-expression [OPTION]... _EXPRESSION

    ARGUMENTS
           _EXPRESSION (required)
               _EXPRESSION is the expression that will be compiled.

    OPTIONS
           --format=DISPLAY_FORMAT, --display-format=DISPLAY_FORMAT
           (absent=human-readable)
               DISPLAY_FORMAT is the format that will be used by the CLI.
               Available formats are 'dev', 'json', and 'human-readable'
               (default). When human-readable lacks details (we are still
               tweaking it), please contact us and use another format in the
               meanwhile.

           --help[=FMT] (default=auto)
               Show this help in format FMT. The value FMT must be one of `auto',
               `pager', `groff' or `plain'. With `auto', the format is `pager` or
               `plain' whenever the TERM env var is `dumb' or undefined.

           --michelson-format=MICHELSON_FORMAT (absent=text)
               MICHELSON_FORMAT is the format that will be used by
               compile-contract for the resulting Michelson. Available formats
               are 'text' (default), 'json' and 'hex'.

           -s SYNTAX, --syntax=SYNTAX (absent=auto)
               SYNTAX is the syntax that will be used. Currently supported
               syntaxes are "pascaligo" and "cameligo". By default, the syntax is
               guessed from the extension (.ligo and .mligo, respectively). |} ] ;
