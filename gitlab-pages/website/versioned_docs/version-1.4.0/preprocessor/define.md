---
id: define
title: "#define"
---

import Syntax from '@theme/Syntax';

The booleans `false` and `true` are predefined. The way to define
_symbols_ (that is the traditional name of those identifiers) consists
in using the `#define` directive, followed by the symbol, like so:

```
#define SYM

#if SYM
This IS copied to the output.
#else
This is NOT copied to the output, except the newline character.
#endif
```

This opens the possibility to use Boolean expressions made of

- `true` and `false` already mentioned;
- `||` for the disjunction ("or");
- `&&` for the conjunction ("and");
- `==` for equality;
- `!=` for inequality;
- `!` for negation;
- `(` and `)` around expressions to specify priorities.

Directives are processed in sequence in the input file. This
preprocessor, like that of `C#`, allows us to _undefine_ a symbol,
that is, giving it the Boolean value `false`, like so:

```
#define SYM
#undef SYM

#if SYM
This is NOT copied to the output, except the newline character.
#else
This IS copied to the output.
#endif
```

The result is

```
# 1 "Tests/undef.txt"






This IS copied to the output.

```

Note: If you wish to redefine a symbol, you must undefine it first.

When we want to write a cascade of conditionals, the preprocessor
enables a shortcut by means of the `#elif` directive, like so:

```
#if ...
...
#elif ...
...
#elif ...
...
#endif
```

Basically, a `#elif` directive is equivalent to `#else` followed by
`#if`, but we only need to close with only one `#endif`.

The rationale for using conditional directives in LIGO is to enable in
a single smart contract several versions of a standard.

```
#if STANDARD_1
...
#elif STANDARD_2
...
#else
#error Standard not implemented
#endif
```

A real life example could be
[Dexter](https://gitlab.com/dexter2tz/dexter2tz/-/blob/febd360cf6df6e090dedbf21b27538681246f980/dexter.mligo#L52). It
provides another interesting use of a conditional directive, where
[a record type depends on the version of the standard](https://gitlab.com/dexter2tz/dexter2tz/-/blob/febd360cf6df6e090dedbf21b27538681246f980/dexter.mligo#L84).
