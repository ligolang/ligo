---
id: comments
title: Comments
---

import Syntax from '@theme/Syntax';

Comments are recognised by the preprocessor, even in pieces of the
input that are not copied. (This last point is a difference between
`cpp` and the `C#` preprocessor.) The rationale for doing so when
copying the input is that we do not want the preprocessor to interpret
a directive that is actually in a comment. This can happen when
commenting out a piece of the source code that contains a
preprocessing directive: we do not want that directive to be
interpreted.

When the processor is in skip mode, that is, the input is not copied,
comments are also recognised. This ensures that a comment containing a
conditional directive, for example `#endif`, does not start to
interact with previous directives, like `#if`, or raises an error when
switching from copy mode to skip mode. In other words, the
interpretation of comments should always be the same. For
example, we want the following input to be valid:

```
#if true
 // #endif
#endif
```

<Syntax syntax="cameligo">

Comments are blocks enclosed between `(*` and `*)`, and start with
`//` for line comments.

</Syntax>

<Syntax syntax="jsligo">

Comments are blocks enclosed between `/*` and `*/`, and start with
`//` for line comments.

</Syntax>
