---
id: strings
title: Strings
---

import Syntax from '@theme/Syntax';

Strings are recognised by the preprocessor, even in pieces of the
input that are not copied. (This last point is a difference between
`cpp` and the `C#` preprocessor.) The rationale for doing so when
copying the input is that we do not want the preprocessor to interpret
a directive that is actually in a string. This can happen if the
source code is that of a bootstrapped compiler, that is, a compiler
for its own language. Another scenario is that of a test: the source
code is actually printing what is happening.

When the processor is in skip mode, that is, the input is not copied,
strings are also recognised. This ensures that a string containing a
conditional directive, for example `#endif`, does not start to
interact with previous directives, like `#if`, or raises an error when
switching from copy mode to skip mode. In other words, the
interpretation of strings should always be the same. For example, we
want the following input to be valid:


```
#if true
"#endif"
#endif
```
