---
id: if
title: "#if"
---

import Syntax from '@theme/Syntax';

The preprocessor enables the conditional copying of its input. At the
start, its default mode is _copy_, meaning that characters in the
input are copied to the output. Conditional directives enable another
mode: _skip_, by means of which the following characters are
discarded, and only newline characters are copied.

Conditional directives follow the familiar syntax of some of their
cousins in programming languages. At the very least,

1. they start with the `#if` directive, followed by a Boolean
   expression as argument,

2. and they are closed by `#endif`.

It is also possible to use

- one `#else` directive before `#endif`;

- a series of `#elif` directive after `#if` (as a short-hand for a
  `#else` immediately followed by an `#if`, except that only one
  `#endif` will close the conditional).

A trivial example would be:

```
#if false
This is NOT copied to the output, except the newline character
#endif
```

where `false` is a predefined symbol acting like a Boolean value. The
output is

```
# 1 "Tests/test.txt"




```

Note what looks like an anonymous preprocessing directive `# 1 "Tests/test.txt"`. We will explain its meaning when presenting
[The Inclusion Directive](#the-inclusion-directive). (Remark: `cpp`
would not output blank lines followed by the end of the file.) Their
use is clearer if we add text before and after the conditional, like
so:

```
---
#if false
A
#endif
---
```

whose preprocessed output is then

```
# 1 "Tests/test.txt"
---



---
```

Here is how to use the `#else` directive:

```
#if false
This is NOT copied to the output, except the newline character.
#else
This IS copied to the output.
#endif
```
