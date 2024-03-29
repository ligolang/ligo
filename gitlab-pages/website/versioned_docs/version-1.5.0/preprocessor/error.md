---
id: error
title: "#error"
---

import Syntax from '@theme/Syntax';

When debugging or putting in place directives in an already existing
input, it is sometimes useful to force the preprocessor to stop and
emit an error. This is possible thanks to the `#error` directive,
which is followed by an error message as free text until the end of
the line, like so:

```
#error Not implemented/tested yet
```
