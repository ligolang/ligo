open Cli_expect

let%expect_test _ =
  (* TODO good? *)
  run_ligo_good [] ;
  [%expect {|
    NAME
           ligo

    SYNOPSIS
           ligo COMMAND ...

    MORE HELP
           Use `ligo COMMAND --help' for help on a single command.

    DOCUMENTATION
           https://ligolang.org/docs/intro/introduction

    ASK A QUESTION
           https://discord.gg/9rhYaEt

    OPEN AN ISSUE
           https://gitlab.com/ligolang/ligo/issues/new

    COMMANDS
           changelog
               Dump the LIGO changelog to stdout.

           compile-contract
               Subcommand: Compile a contract.

           compile-expression
               Subcommand: Compile to a Michelson value.

           compile-parameter
               Subcommand: Compile parameters to a Michelson expression.

           compile-storage
               Subcommand: Compile an initial storage in LIGO syntax to a
               Michelson expression.

           dry-run
               Subcommand: Run a smart-contract with the given storage and input.

           evaluate-call
               Subcommand: Run a function with the given parameter.

           evaluate-expr
               Subcommand: Evaluate a given definition.

           evaluate-value
               Deprecated, renamed to evaluate-expr. Use evaluate-expr instead.
               Subcommand: Evaluate a given definition.

           get-scope
               Subcommand: Return the JSON encoded environment for a given file.

           interpret
               Subcommand: Interpret the expression in the context initialized by
               the provided source file.

           list-declarations
               Subcommand: List all the top-level declarations.

           measure-contract
               Subcommand: Measure a contract's compiled size in bytes.

           mutate-ast
               Subcommand: Return a mutated version for a given file.

           mutate-cst
               Subcommand: Return a mutated version for a given file.

           preprocess
               Subcommand: Preprocess the source file. Warning: Intended for
               development of LIGO and can break at any time.

           pretty-print
               Subcommand: Pretty-print the source file.

           print-ast
               Subcommand: Print the AST. Warning: Intended for development of
               LIGO and can break at any time.

           print-ast-combined
               Subcommand: Print the contract after combination with the build
               system. Warning: Intended for development of LIGO and can break at
               any time.

           print-ast-core
               Subcommand: Print the AST. Warning: Intended for development of
               LIGO and can break at any time.

           print-ast-sugar
               Subcommand: Print the AST. Warning: Intended for development of
               LIGO and can break at any time.

           print-ast-typed
               Subcommand: Print the typed AST. Warning: Intended for development
               of LIGO and can break at any time.

           print-cst
               Subcommand: Print the CST. Warning: Intended for development of
               LIGO and can break at any time.

           print-graph
               Subcommand: Print the dependency graph. Warning: Intended for
               development of LIGO and can break at any time.

           print-mini-c
               Subcommand: Print Mini-C. Warning: Intended for development of
               LIGO and can break at any time.

           repl
               Subcommand: REPL

           run-function
               Deprecated, renamed to evaluate-call. Use evaluate-call instead.
               Subcommand: Run a function with the given parameter.

           test
               Subcommand: Test a contract with the LIGO test framework (BETA).

           transpile-contract
               Subcommand: Transpile a contract to another syntax (BETA).

           transpile-expression
               Subcommand: Transpile an expression to another syntax (BETA).

    OPTIONS
           --help[=FMT] (default=auto)
               Show this help in format FMT. The value FMT must be one of `auto',
               `pager', `groff' or `plain'. With `auto', the format is `pager` or
               `plain' whenever the TERM env var is `dumb' or undefined.

           --version
               Show version information. |} ] ;

  run_ligo_good [ "--help" ] ;
  [%expect {|
    NAME
           ligo

    SYNOPSIS
           ligo COMMAND ...

    MORE HELP
           Use `ligo COMMAND --help' for help on a single command.

    DOCUMENTATION
           https://ligolang.org/docs/intro/introduction

    ASK A QUESTION
           https://discord.gg/9rhYaEt

    OPEN AN ISSUE
           https://gitlab.com/ligolang/ligo/issues/new

    COMMANDS
           changelog
               Dump the LIGO changelog to stdout.

           compile-contract
               Subcommand: Compile a contract.

           compile-expression
               Subcommand: Compile to a Michelson value.

           compile-parameter
               Subcommand: Compile parameters to a Michelson expression.

           compile-storage
               Subcommand: Compile an initial storage in LIGO syntax to a
               Michelson expression.

           dry-run
               Subcommand: Run a smart-contract with the given storage and input.

           evaluate-call
               Subcommand: Run a function with the given parameter.

           evaluate-expr
               Subcommand: Evaluate a given definition.

           evaluate-value
               Deprecated, renamed to evaluate-expr. Use evaluate-expr instead.
               Subcommand: Evaluate a given definition.

           get-scope
               Subcommand: Return the JSON encoded environment for a given file.

           interpret
               Subcommand: Interpret the expression in the context initialized by
               the provided source file.

           list-declarations
               Subcommand: List all the top-level declarations.

           measure-contract
               Subcommand: Measure a contract's compiled size in bytes.

           mutate-ast
               Subcommand: Return a mutated version for a given file.

           mutate-cst
               Subcommand: Return a mutated version for a given file.

           preprocess
               Subcommand: Preprocess the source file. Warning: Intended for
               development of LIGO and can break at any time.

           pretty-print
               Subcommand: Pretty-print the source file.

           print-ast
               Subcommand: Print the AST. Warning: Intended for development of
               LIGO and can break at any time.

           print-ast-combined
               Subcommand: Print the contract after combination with the build
               system. Warning: Intended for development of LIGO and can break at
               any time.

           print-ast-core
               Subcommand: Print the AST. Warning: Intended for development of
               LIGO and can break at any time.

           print-ast-sugar
               Subcommand: Print the AST. Warning: Intended for development of
               LIGO and can break at any time.

           print-ast-typed
               Subcommand: Print the typed AST. Warning: Intended for development
               of LIGO and can break at any time.

           print-cst
               Subcommand: Print the CST. Warning: Intended for development of
               LIGO and can break at any time.

           print-graph
               Subcommand: Print the dependency graph. Warning: Intended for
               development of LIGO and can break at any time.

           print-mini-c
               Subcommand: Print Mini-C. Warning: Intended for development of
               LIGO and can break at any time.

           repl
               Subcommand: REPL

           run-function
               Deprecated, renamed to evaluate-call. Use evaluate-call instead.
               Subcommand: Run a function with the given parameter.

           test
               Subcommand: Test a contract with the LIGO test framework (BETA).

           transpile-contract
               Subcommand: Transpile a contract to another syntax (BETA).

           transpile-expression
               Subcommand: Transpile an expression to another syntax (BETA).

    OPTIONS
           --help[=FMT] (default=auto)
               Show this help in format FMT. The value FMT must be one of `auto',
               `pager', `groff' or `plain'. With `auto', the format is `pager` or
               `plain' whenever the TERM env var is `dumb' or undefined.

           --version
               Show version information. |} ] ;

  run_ligo_good [ "compile-contract" ; "--help" ] ;
  [%expect {|
    NAME
           ligo-compile-contract - Subcommand: Compile a contract.

    SYNOPSIS
           ligo compile-contract [OPTION]... SOURCE_FILE ENTRY_POINT

    DESCRIPTION
           This sub-command compiles a contract to Michelson code. It expects a
           source file and an entrypoint function that has the type of a
           contract: "parameter * storage -> operations list * storage".

    ARGUMENTS
           ENTRY_POINT (required)
               ENTRY_POINT is entry-point that will be compiled.

           SOURCE_FILE (required)
               SOURCE_FILE is the path to the smart contract file.

    OPTIONS
           --disable-michelson-typechecking
               disable Michelson typecking, this might produce ill-typed
               Michelson code.

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

           --infer
               enable type inference

           --michelson-format=MICHELSON_FORMAT (absent=text)
               MICHELSON_FORMAT is the format that will be used by
               compile-contract for the resulting Michelson. Available formats
               are 'text' (default), 'json' and 'hex'.

           --output-file=OUTPUT_FILE, --output=OUTPUT_FILE
               OUTPUT_FILE if used, prints the output into the specified file
               instead of stdout

           -p PROTOCOL_VERSION, --protocol=PROTOCOL_VERSION (absent=current)
               PROTOCOL_VERSION will decide protocol's types/values pre-loaded
               into the LIGO environment (edo). By default, the current protocol
               (edo) will be used

           -s SYNTAX, --syntax=SYNTAX (absent=auto)
               SYNTAX is the syntax that will be used. Currently supported
               syntaxes are "pascaligo", "cameligo", "reasonligo" and "jsligo".
               By default, the syntax is guessed from the extension (.ligo,
               .mligo, .religo, and .jsligo respectively).

           --version
               Show version information.

           --warn=BOOL (absent=true)
               BOOL indicates whether warning messages should be printed in
               stderr or not

           --werror=BOOL (absent=false)
               BOOL indicates whether warning messages should be treated as
               errors or not |} ] ;

  run_ligo_good [ "compile-parameter" ; "--help" ] ;
  [%expect {|
    NAME
           ligo-compile-parameter - Subcommand: Compile parameters to a Michelson
           expression.

    SYNOPSIS
           ligo compile-parameter [OPTION]... SOURCE_FILE ENTRY_POINT
           PARAMETER_EXPRESSION

    DESCRIPTION
           This sub-command compiles a parameter for a given contract to a
           Michelson expression. The resulting Michelson expression can be passed
           as an argument in a transaction which calls a contract.

    ARGUMENTS
           ENTRY_POINT (required)
               ENTRY_POINT is entry-point that will be compiled.

           PARAMETER_EXPRESSION (required)
               PARAMETER_EXPRESSION is the expression that will be compiled.

           SOURCE_FILE (required)
               SOURCE_FILE is the path to the smart contract file.

    OPTIONS
           --amount=AMOUNT (absent=0)
               AMOUNT is the amount the Michelson interpreter will use for the
               transaction.

           --balance=BALANCE (absent=0)
               BALANCE is the balance the Michelson interpreter will use for the
               contract balance.

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

           --infer
               enable type inference

           --michelson-format=MICHELSON_FORMAT (absent=text)
               MICHELSON_FORMAT is the format that will be used by
               compile-contract for the resulting Michelson. Available formats
               are 'text' (default), 'json' and 'hex'.

           --now=NOW
               NOW is the NOW value the Michelson interpreter will use (e.g.
               '2000-01-01T10:10:10Z')

           --output-file=OUTPUT_FILE, --output=OUTPUT_FILE
               OUTPUT_FILE if used, prints the output into the specified file
               instead of stdout

           -p PROTOCOL_VERSION, --protocol=PROTOCOL_VERSION (absent=current)
               PROTOCOL_VERSION will decide protocol's types/values pre-loaded
               into the LIGO environment (edo). By default, the current protocol
               (edo) will be used

           -s SYNTAX, --syntax=SYNTAX (absent=auto)
               SYNTAX is the syntax that will be used. Currently supported
               syntaxes are "pascaligo", "cameligo", "reasonligo" and "jsligo".
               By default, the syntax is guessed from the extension (.ligo,
               .mligo, .religo, and .jsligo respectively).

           --sender=SENDER
               SENDER is the sender the Michelson interpreter transaction will
               use.

           --source=SOURCE
               SOURCE is the source the Michelson interpreter transaction will
               use.

           --version
               Show version information.

           --warn=BOOL (absent=true)
               BOOL indicates whether warning messages should be printed in
               stderr or not

           --werror=BOOL (absent=false)
               BOOL indicates whether warning messages should be treated as
               errors or not |} ] ;

  run_ligo_good [ "compile-storage" ; "--help" ] ;
  [%expect {|
    NAME
           ligo-compile-storage - Subcommand: Compile an initial storage in LIGO
           syntax to a Michelson expression.

    SYNOPSIS
           ligo compile-storage [OPTION]... SOURCE_FILE ENTRY_POINT
           STORAGE_EXPRESSION

    DESCRIPTION
           This sub-command compiles an initial storage for a given contract to a
           Michelson expression. The resulting Michelson expression can be passed
           as an argument in a transaction which originates a contract.

    ARGUMENTS
           ENTRY_POINT (required)
               ENTRY_POINT is entry-point that will be compiled.

           SOURCE_FILE (required)
               SOURCE_FILE is the path to the smart contract file.

           STORAGE_EXPRESSION (required)
               STORAGE_EXPRESSION is the expression that will be compiled.

    OPTIONS
           --amount=AMOUNT (absent=0)
               AMOUNT is the amount the Michelson interpreter will use for the
               transaction.

           --balance=BALANCE (absent=0)
               BALANCE is the balance the Michelson interpreter will use for the
               contract balance.

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

           --infer
               enable type inference

           --michelson-format=MICHELSON_FORMAT (absent=text)
               MICHELSON_FORMAT is the format that will be used by
               compile-contract for the resulting Michelson. Available formats
               are 'text' (default), 'json' and 'hex'.

           --now=NOW
               NOW is the NOW value the Michelson interpreter will use (e.g.
               '2000-01-01T10:10:10Z')

           --output-file=OUTPUT_FILE, --output=OUTPUT_FILE
               OUTPUT_FILE if used, prints the output into the specified file
               instead of stdout

           -p PROTOCOL_VERSION, --protocol=PROTOCOL_VERSION (absent=current)
               PROTOCOL_VERSION will decide protocol's types/values pre-loaded
               into the LIGO environment (edo). By default, the current protocol
               (edo) will be used

           -s SYNTAX, --syntax=SYNTAX (absent=auto)
               SYNTAX is the syntax that will be used. Currently supported
               syntaxes are "pascaligo", "cameligo", "reasonligo" and "jsligo".
               By default, the syntax is guessed from the extension (.ligo,
               .mligo, .religo, and .jsligo respectively).

           --sender=SENDER
               SENDER is the sender the Michelson interpreter transaction will
               use.

           --source=SOURCE
               SOURCE is the source the Michelson interpreter transaction will
               use.

           --version
               Show version information.

           --warn=BOOL (absent=true)
               BOOL indicates whether warning messages should be printed in
               stderr or not

           --werror=BOOL (absent=false)
               BOOL indicates whether warning messages should be treated as
               errors or not |} ] ;

  run_ligo_good [ "dry-run" ; "--help" ] ;
  [%expect {|
    NAME
           ligo-dry-run - Subcommand: Run a smart-contract with the given storage
           and input.

    SYNOPSIS
           ligo dry-run [OPTION]... SOURCE_FILE ENTRY_POINT PARAMETER_EXPRESSION
           STORAGE_EXPRESSION

    DESCRIPTION
           This sub-command runs a LIGO contract on a given storage and
           parameter. The context is initialized from a source file where the
           contract is implemented. The interpretation is done using Michelson's
           interpreter.

    ARGUMENTS
           ENTRY_POINT (required)
               ENTRY_POINT is entry-point that will be compiled.

           PARAMETER_EXPRESSION (required)
               PARAMETER_EXPRESSION is the expression that will be compiled.

           SOURCE_FILE (required)
               SOURCE_FILE is the path to the smart contract file.

           STORAGE_EXPRESSION (required)
               STORAGE_EXPRESSION is the expression that will be compiled.

    OPTIONS
           --amount=AMOUNT (absent=0)
               AMOUNT is the amount the Michelson interpreter will use for the
               transaction.

           --balance=BALANCE (absent=0)
               BALANCE is the balance the Michelson interpreter will use for the
               contract balance.

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

           --infer
               enable type inference

           --now=NOW
               NOW is the NOW value the Michelson interpreter will use (e.g.
               '2000-01-01T10:10:10Z')

           -p PROTOCOL_VERSION, --protocol=PROTOCOL_VERSION (absent=current)
               PROTOCOL_VERSION will decide protocol's types/values pre-loaded
               into the LIGO environment (edo). By default, the current protocol
               (edo) will be used

           -s SYNTAX, --syntax=SYNTAX (absent=auto)
               SYNTAX is the syntax that will be used. Currently supported
               syntaxes are "pascaligo", "cameligo", "reasonligo" and "jsligo".
               By default, the syntax is guessed from the extension (.ligo,
               .mligo, .religo, and .jsligo respectively).

           --sender=SENDER
               SENDER is the sender the Michelson interpreter transaction will
               use.

           --source=SOURCE
               SOURCE is the source the Michelson interpreter transaction will
               use.

           --version
               Show version information.

           --warn=BOOL (absent=true)
               BOOL indicates whether warning messages should be printed in
               stderr or not

           --werror=BOOL (absent=false)
               BOOL indicates whether warning messages should be treated as
               errors or not |} ] ;

  run_ligo_good [ "evaluate-call" ; "--help" ] ;
  [%expect {|
    NAME
           ligo-evaluate-call - Subcommand: Run a function with the given
           parameter.

    SYNOPSIS
           ligo evaluate-call [OPTION]... SOURCE_FILE ENTRY_POINT
           PARAMETER_EXPRESSION

    DESCRIPTION
           This sub-command runs a LIGO function on a given argument. The context
           is initialized from a source file where the function is implemented.
           The interpretation is done using Michelson's interpreter.

    ARGUMENTS
           ENTRY_POINT (required)
               ENTRY_POINT is entry-point that will be compiled.

           PARAMETER_EXPRESSION (required)
               PARAMETER_EXPRESSION is the expression that will be compiled.

           SOURCE_FILE (required)
               SOURCE_FILE is the path to the smart contract file.

    OPTIONS
           --amount=AMOUNT (absent=0)
               AMOUNT is the amount the Michelson interpreter will use for the
               transaction.

           --balance=BALANCE (absent=0)
               BALANCE is the balance the Michelson interpreter will use for the
               contract balance.

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

           --infer
               enable type inference

           --now=NOW
               NOW is the NOW value the Michelson interpreter will use (e.g.
               '2000-01-01T10:10:10Z')

           -p PROTOCOL_VERSION, --protocol=PROTOCOL_VERSION (absent=current)
               PROTOCOL_VERSION will decide protocol's types/values pre-loaded
               into the LIGO environment (edo). By default, the current protocol
               (edo) will be used

           -s SYNTAX, --syntax=SYNTAX (absent=auto)
               SYNTAX is the syntax that will be used. Currently supported
               syntaxes are "pascaligo", "cameligo", "reasonligo" and "jsligo".
               By default, the syntax is guessed from the extension (.ligo,
               .mligo, .religo, and .jsligo respectively).

           --sender=SENDER
               SENDER is the sender the Michelson interpreter transaction will
               use.

           --source=SOURCE
               SOURCE is the source the Michelson interpreter transaction will
               use.

           --version
               Show version information.

           --warn=BOOL (absent=true)
               BOOL indicates whether warning messages should be printed in
               stderr or not

           --werror=BOOL (absent=false)
               BOOL indicates whether warning messages should be treated as
               errors or not |} ] ;

  run_ligo_good [ "evaluate-expr" ; "--help" ] ;
  [%expect {|
    NAME
           ligo-evaluate-expr - Subcommand: Evaluate a given definition.

    SYNOPSIS
           ligo evaluate-expr [OPTION]... SOURCE_FILE ENTRY_POINT

    DESCRIPTION
           This sub-command evaluates a LIGO definition. The context is
           initialized from a source file where the definition is written. The
           interpretation is done using a Michelson interpreter.

    ARGUMENTS
           ENTRY_POINT (required)
               ENTRY_POINT is entry-point that will be compiled.

           SOURCE_FILE (required)
               SOURCE_FILE is the path to the smart contract file.

    OPTIONS
           --amount=AMOUNT (absent=0)
               AMOUNT is the amount the Michelson interpreter will use for the
               transaction.

           --balance=BALANCE (absent=0)
               BALANCE is the balance the Michelson interpreter will use for the
               contract balance.

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

           --infer
               enable type inference

           --now=NOW
               NOW is the NOW value the Michelson interpreter will use (e.g.
               '2000-01-01T10:10:10Z')

           -p PROTOCOL_VERSION, --protocol=PROTOCOL_VERSION (absent=current)
               PROTOCOL_VERSION will decide protocol's types/values pre-loaded
               into the LIGO environment (edo). By default, the current protocol
               (edo) will be used

           -s SYNTAX, --syntax=SYNTAX (absent=auto)
               SYNTAX is the syntax that will be used. Currently supported
               syntaxes are "pascaligo", "cameligo", "reasonligo" and "jsligo".
               By default, the syntax is guessed from the extension (.ligo,
               .mligo, .religo, and .jsligo respectively).

           --sender=SENDER
               SENDER is the sender the Michelson interpreter transaction will
               use.

           --source=SOURCE
               SOURCE is the source the Michelson interpreter transaction will
               use.

           --version
               Show version information.

           --warn=BOOL (absent=true)
               BOOL indicates whether warning messages should be printed in
               stderr or not

           --werror=BOOL (absent=false)
               BOOL indicates whether warning messages should be treated as
               errors or not |} ] ;

  run_ligo_good [ "compile-expression" ; "--help" ] ;
  [%expect {|
    NAME
           ligo-compile-expression - Subcommand: Compile to a Michelson value.

    SYNOPSIS
           ligo compile-expression [OPTION]... SYNTAX _EXPRESSION

    DESCRIPTION
           This sub-command compiles a LIGO expression to a Michelson value. It
           works by compiling the LIGO expression to a Michelson expression and
           then interpreting it using Michelson's interpreter.

    ARGUMENTS
           _EXPRESSION (required)
               _EXPRESSION is the expression that will be compiled.

           SYNTAX (required)
               SYNTAX is the syntax that will be used. Currently supported
               syntaxes are "pascaligo", "cameligo" and "reasonligo". By default,
               the syntax is guessed from the extension (.ligo, .mligo, .religo,
               .jsligo respectively).

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

           --infer
               enable type inference

           --init-file=INIT_FILE
               INIT_FILE is the path to smart contract file to be used for
               context initialization.

           --michelson-format=MICHELSON_FORMAT (absent=text)
               MICHELSON_FORMAT is the format that will be used by
               compile-contract for the resulting Michelson. Available formats
               are 'text' (default), 'json' and 'hex'.

           -p PROTOCOL_VERSION, --protocol=PROTOCOL_VERSION (absent=current)
               PROTOCOL_VERSION will decide protocol's types/values pre-loaded
               into the LIGO environment (edo). By default, the current protocol
               (edo) will be used

           --version
               Show version information.

           --warn=BOOL (absent=true)
               BOOL indicates whether warning messages should be printed in
               stderr or not

           --werror=BOOL (absent=false)
               BOOL indicates whether warning messages should be treated as
               errors or not

           --without-run
               disable running of compiled expression. |} ] ;
