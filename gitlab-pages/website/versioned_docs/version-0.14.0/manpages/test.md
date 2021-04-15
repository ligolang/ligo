### NAME

ligo-test - Subcommand: Test a contract with the LIGO interpreter
(BETA).

### SYNOPSIS

**ligo test** \[*OPTION*\]\... *SOURCE_FILE* *TEST_ENTRY*

### DESCRIPTION

This sub-command tests a LIGO contract using a LIGO interpreter, no
Michelson code is evaluated. Still under development, there are features
that are work in progress and are subject to change. No real test
procedure should rely on this sub-command alone.

<!-- 
TODO: correct text below

### EXTRA PRIMITIVES FOR TESTING

Test.originate c st : binds contract c with the address addr which is
returned, st as the initial storage.

Test.set_now t : sets the current time to t.

Test.set_balance addr b : sets the balance of contract bound to address
addr (returns unit).

Test.external_call addr p amt : performs a call to contract bound to
addr with parameter p and amount amt (returns unit).

Test.get_storage addr : returns current storage bound to address addr.

Test.get_balance : returns current balance bound to address addr.

Test.assert_failure (f : unit -\> \_) : returns true if f () fails.

Test.log x : prints x into the console.

### ARGUMENTS

*SOURCE_FILE* (required)

:   *SOURCE_FILE* is the path to the smart contract file.

*TEST_ENTRY* (required)

:   *TEST_ENTRY* is top-level variable which will be evaluated as the
    result of your test.

### OPTIONS

**\--amount**=*AMOUNT* (absent=0)

:   *AMOUNT* is the amount the Michelson interpreter will use for the
    transaction.

**\--balance**=*BALANCE* (absent=0)

:   *BALANCE* is the balance the Michelson interpreter will use for the
    contract balance.

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

:   enable type inferance

**\--now**=*NOW*

:   *NOW* is the NOW value the Michelson interpreter will use (e.g.
    \`2000-01-01T10:10:10Z\`)

**-p** *PROTOCOL_VERSION*, **\--protocol**=*PROTOCOL_VERSION* (absent=current)

:   *PROTOCOL_VERSION* will decide protocol\`s types/values pre-loaded
    into the LIGO environment (edo). By default, the current protocol
    (edo) will be used

**-s** *SYNTAX*, **\--syntax**=*SYNTAX* (absent=auto)

:   *SYNTAX* is the syntax that will be used. Currently supported
    syntaxes are \"pascaligo\", \"cameligo\" and \"reasonligo\". By
    default, the syntax is guessed from the extension (.ligo, .mligo,
    .religo respectively).

**\--sender**=*SENDER*

:   *SENDER* is the sender the Michelson interpreter transaction will
    use.

**\--source**=*SOURCE*

:   *SOURCE* is the source the Michelson interpreter transaction will
    use.

**\--version**

:   Show version information. -->
