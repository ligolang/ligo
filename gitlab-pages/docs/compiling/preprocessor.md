---
id: preprocessor
title: Preprocessor
---

import Syntax from '@theme/Syntax';

The preprocessor edits files before they go to the LIGO compiler.

<Syntax syntax="jsligo">

The JsLIGO preprocessor no longer supports preprocessor directives.

- Instead of using the `#include` or `#import` directives, import namespaces in other files directly with the `import` keyword as described in [Importing and using classes](../syntax/classes#importing-and-using-classes) or [Importing namespaces](../syntax/modules#importing-namespaces).

- The `#if`, `#else`, `#elif`, `#endif`, `#define`, `#undef`, and `#error` directives are no longer supported.
If you need to continue using them, you can run your JsLIGO code through a C++ preprocessor, which uses the same syntax.
JsLIGO code with these directives does not compile.

</Syntax>

<Syntax syntax="cameligo">

CameLIGO code can include commands called _preprocessor directives_ to instruct the preprocessor to make changes to a file before the compiler receives it, such as including or excluding code and importing code from other files.

Preprocessor directives can allow you to make changes to files before the compiler processes them.
For example, the following contract has three entrypoints, but one is between `#if` and `#endif` directives.
The line `#if INCLUDE_RESET` instructs the preprocessor to include the text between the directives (in this case, the third entrypoint) only if the `INCLUDE_RESET` Boolean variable is set:

```cameligo group=includereset
module MyContract = struct
  type storage = int
  type result = operation list * storage

  [@entry] let increment (delta : int) (storage : storage) : result = [],storage + delta

  [@entry] let decrement (delta : int) (storage : storage) : result = [],storage - delta

  #if INCLUDE_RESET
  [@entry] let reset () (_storage : storage) : result = [], 0
  #endif
end
```

You can set these Boolean preprocessor variables with the [`#define`](#define-and-undef) directive or by passing them to the `-D` argument of the `ligo compile contract` command.
For example, if the contract in the previous example is in a file named `mycontract.mligo`, this command causes the preprocessor and compiler to output a contract with only two entrypoints:

```bash
ligo compile contract mycontract.mligo
```

This command passes the `INCLUDE_RESET` Boolean variable to the preprocessor and causes the compiler to output a contract with three entrypoints:

```bash
ligo compile contract -D INCLUDE_RESET mycontract.mligo
```

</Syntax>

## Viewing the preprocessor output

It's rarely necessary to view the output of the preprocessor, but if you need to see the output to debug directives, you can view the output with the `ligo print preprocessed` command, as in this example:

<Syntax syntax="cameligo">

```bash
ligo print preprocessed myContract.mligo
```

</Syntax>

<Syntax syntax="jsligo">

```bash
ligo print preprocessed myContract.jsligo
```

</Syntax>

<Syntax syntax="cameligo">

## Comments

The preprocessor ignores directives that are in [comments](../syntax/comments), which prevents problems where comments in your code contain text that looks like a directive.
For example, this code is valid because the preprocessor ignores the text `#endif` in the comment:

```
#if true
 // #endif
#endif
```

The preprocessor includes and excludes comments just like any other line of code.

## String processing

The preprocessor ignores directives that are in strings, which prevents problems where strings in your code contain text that looks like a directive.

For example, this code includes a string with the text `#endif`, but the preprocessor does not interpret this text as the `#endif` directive:

```cameligo skip
#if true
let textValue = "This string includes the text #endif"
#endif
```

## Blank lines

When the preprocessor hides text, it includes a blank line consisting only of a newline character in place of the omitted line to keep line numbers in compiler errors consistent with the source file.
Similarly, it includes a blank line in place of each preprocessor directive.
These blank lines do not affect compilation.

For example, take this source file:

```
#if false
This is NOT copied to the output, except the newline character
#endif
```

The output that goes to the compiler from this source file is three blank lines plus the linemarker that indicates the start of the file:

```
# 1 "mySourceFile.txt"



```

## Linemarkers

As in C-based languages, the LIGO preprocessor includes [linemarkers](https://gcc.gnu.org/onlinedocs/cpp/Preprocessor-Output.html) in files that it processes.
For example, if you include files with the `#include` directive, the preprocessor inserts a line in the processed file to indicate where the included file starts and a line to indicate where the included file ends and the original file resumes.

The output begins with a linemarker that indicates the start of the original file that was sent to the preprocessor, in the format `# <line number> "<path to file>"`.
Other linemarkers follow the same format, adding the number `1` to indicate the start of an imported file and the number `2` to indicate the end of the imported file and the resumption of the previous file.

The following example uses three files: `a.txt`, `b.txt`, and `c.txt`.
File A includes file B and file B includes file C.

```
Start of "a.txt"
#include "b.txt"
End of "a.txt"
```

```
Start of "b.txt"
#include "c.txt"
End of "b.txt"
```

```
Start of "c.txt"
End of "c.txt"
```

If you create these files and run the command `ligo print preprocessed --syntax cameligo a.txt`, the output includes linemarkers that indicate where the files begin and end and the line number in the file that the preprocessor is on at the time:

```
# 1 "a.txt"
Start of "a.txt"

# 1 "b.txt" 1
Start of "b.txt"

# 1 "c.txt" 1
Start of "c.txt"
End of "c.txt"
# 3 "b.txt" 2
End of "b.txt"
# 3 "a.txt" 2
End of "a.txt"
```

The LIGO compiler ignores these linemarkers when it compiles the code.

## Directives

These are the preprocessor directives that the CameLIGO preprocessor supports:

- [`#define` and `#undef`](#define-and-undef)
- [`#error`](#error)
- [`#if`, `#else`, `#elif`, and `#endif`](#if-else-elif-and-endif)
- [`#import`](#import)
- [`#include`](#include)

### `#define` and `#undef`

The `#define` directive sets a Boolean variable (also known as a _symbol_) to true, and the `#undef` directive unsets it, which is equivalent to setting it to false.

You can use these variables with the `#if` directive to show or hide text from the compiler, as in this example:

```
#define SYM

#if SYM
This IS copied to the output because SYM is set.
#else
This is NOT copied to the output, except the newline character.
#endif
```

### `#error`

The `#error` directive forces the preprocessor to stop and emit an error.
This directive can help you catch problems in complex files.
You can include an error message, as in this example:

```
#error Not implemented/tested yet
```

### `#if`, `#else`, `#elif`, and `#endif`

These conditional directives allow you to include or exclude text conditionally.
They use a syntax similar to conditions in many other languages, starting with `#if` and ending with `#endif`.
Logic between these two directives can also include:

- One `#else` directive before `#endif`

- One or more `#elif` directives as a shorthand for a
  `#else` immediately followed by an `#if`

For example:

```
#if CONDITION_A
CONDITION_A is true
#elif CONDITION_B
CONDITION_B is true
#else
Neither A nor B are true
#endif
```

The `#if` and `#elif` directives support basic Boolean logic, including:

- `||` for the disjunction ("or")
- `&&` for the conjunction ("and")
- `==` for equality
- `!=` for inequality
- `!` for negation
- `(` and `)` around expressions to specify order of operations

For real-world examples of this logic, see [Dexter](https://gitlab.com/dexter2tz/dexter2tz/-/blob/febd360cf6df6e090dedbf21b27538681246f980/dexter.mligo#L52).
In one section, it uses different record types depending on the version of the FA standard in use (see [`dexter.mligo` line 84](https://gitlab.com/dexter2tz/dexter2tz/-/blob/febd360cf6df6e090dedbf21b27538681246f980/dexter.mligo#L84)):

```cameligo
type storage =
  [@layout:comb]
  { tokenPool : nat ;
    xtzPool : tez ;
    lqtTotal : nat ;
    selfIsUpdatingTokenPool : bool ;
    freezeBaker : bool ;
    manager : address ;
    tokenAddress : address ;
#if FA2
    tokenId : nat ;
#endif
    lqtAddress : address ;
  }
```

### `#import`

The `#import` directive prompts the preprocessor to include another file as a [module](../syntax/modules) in the current file.

For example, you can create a file with related type definitions, as in this example file named `euro.mligo`:

```cameligo group=euro
type t = nat

let add (a, b : t * t) : t = a + b

let one : t = 1n
let two : t = 2n
```

In another file, you can import this file, assign it the module `Euro`, and use its definitions:

```cameligo group=main_importer
module Euro = Gitlab_pages.Docs.Compiling.Src.Preprocessor.Euro

type storage = Euro.t

let tip (s : storage) : storage = Euro.add (s, Euro.one)
```

For more information, see [Modules](../syntax/modules).

### `#include`

The `#include` directive includes the entire text contents of the specified file, as in this example:

```
#include "path/to/standard_1.mligo"
```

Unlike the `#import` directive, the `#include` directive does not package the included file as a module.

</Syntax>
