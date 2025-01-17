---
id: public
title: public
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">

The attribute `[@public]` labels a declaration as available to be called by other pieces of code.
Because code is public by default in CameLIGO, this attribute is not needed in that syntax.

</Syntax>

<Syntax syntax="jsligo">

The decorator `@public` labels a declaration as available to be called by other pieces of code, similar to the `export` keyword.
This decorator is needed only when you want to make a namespace available to code in other files, as described in [Importing namespaces](../../preprocessor/import#importing-namespaces).

</Syntax>
