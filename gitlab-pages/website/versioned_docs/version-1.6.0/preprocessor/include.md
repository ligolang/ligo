---
id: include
title: "#include"
---

import Syntax from '@theme/Syntax';

The solution provided by the conditional directives with symbol
definition to manage several standards is improved upon by physically
separating the input into different files. This is where the
`#include` directive comes handy. Basically, it takes an argument made
of a string containing a path to the file to be textually included,
like so:

```
#include "path/to/standard_1.ligo"
```

and the preprocessor replaces the directive with the contents of the
file `path/to/standard_1.ligo`, whose contents is then preprocessed as
well. This can in theory create a loop, for example, if two files try
to include each other.

In fact, the preprocessor does more than simply include the given
file. To enable the consumer of the output to keep track of
inclusions, in particular, to maintain the line numbers of the input
that has been copied, the preprocessor inserts two special directives
in the output, called
[linemarkers](https://gcc.gnu.org/onlinedocs/cpp/Preprocessor-Output.html),
one in the stead of the `#include` directive and one after the
inclusion. Let us consider the following example where `a.txt`
includes `b.txt`, which, in turn, includes `c.txt`. Here is the
contents of `a.txt`:

```
Start of "a.txt"
#include "b.txt"
End of "a.txt"
```

Then `b.txt`:

```
Start of "b.txt"
#include "c.txt"
End of "b.txt"
```

and, finally, `c.txt`:

```
Start of "c.txt"
End of "c.txt"
```

If we gather the files in a `Tests` directory and run the preprocessor
only, like so

> $ ligo print preprocessed Tests/a.txt

we obtain on `stdout`:

```
# 1 "Tests/a.txt"
Start of "a.txt"

# 1 "Tests/b.txt" 1
Start of "b.txt"

# 1 "Tests/c.txt" 1
Start of "c.txt"
End of "c.txt"
# 3 "Tests/b.txt" 2
End of "b.txt"
# 3 "Tests/a.txt" 2
End of "a.txt"
```

There are three forms of linemarkers:

1. `# <line number> "path/to/file"`
2. `# <line number> "path/to/file" 1`
3. `# <line number> "path/to/file" 2`

The first kind is used only at the start of the output file and states
that the line after the linemarker has number `<line number>` and
belongs to the file `path/to/file`. Therefore `Start of "a.txt"` has
line number `1` in file `Tests/a.txt`.

The second kind is used when including a file. The `#include`
directive is discarded in the output, except the newline character,
which explains the empty line after `Start of "a.txt"`. Then a
linemarker ending in `1` is printed, which means that we went to the
file `path/to/file` when processing the input.

The third kind is inserted in the output upon returning from an
included file. For example, `# 3 "Tests/b.txt" 2` means that the next
line has number `3` and we return to file `Tests/b.txt`.

Linemarkers need to be handled by the consumer of the output. In the
context of the LIGO compiler, the lexer reads the output of the
preprocessor, therefore scans for linemarkers.

When using the preprocessor with the LIGO compiler, the `#include`
directive can only occur at the top level according to the grammar,
that is, either at the beginning of the smart contract, in between
file-level declarations or at the end. (This property is checked by
the parser.) The rationale for this restriction is to avoid fragments
of smart contracts that are syntactically incorrect, and yet assembled
into a correct one.
