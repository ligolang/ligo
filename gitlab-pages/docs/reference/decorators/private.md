---
id: private
title: private
---

import Syntax from '@theme/Syntax'

<Syntax syntax="cameligo">

The attribute `[@private]` can be used on a top-level declaration to
prevent a given value from being exported outside the compilation
unit.

Consider the following contents of the file `module-with-private.mligo`:

```cameligo group=module-with-private
[@private] let stuff = 42
[@private] let g x = x * stuff
let f x = g x + 1 // exported by default
```

Then the following piece of code, in another file:

```cameligo group=import-module-with-private
module ModuleWithPrivate = Gitlab_pages.Docs.Reference.Decorators.Src.Private.Module_with_private

let foo = ModuleWithPrivate.f 123  // = 5167

(*
  The following lines cause errors because g and stuff are private:

  let bad_1 = ModuleWithPrivate.g 123
  let bad_2 = ModuleWithPrivate.stuff
*)
```

</Syntax>

<Syntax syntax="jsligo">

The decorator `@private` can be used on a top-level declaration to
prevent a given value from being exported outside the compilation
unit.

Consider the following contents of the file `module-with-private.jsligo`:

```jsligo group=module-with-private
@private const stuff = 42;
@private const g = x => x * stuff;
const f = x => g(x) + 1; // exported by default
```

Then the following piece of code, in another file:

```jsligo group=import-module-with-private
import * as ModuleWithPrivate from "gitlab-pages/docs/reference/decorators/src/private/module-with-private.mligo";

const foo = ModuleWithPrivate.f(123);  // = 5167

/*
  The following lines cause errors because g and stuff are private:

  const bad_1 = ModuleWithPrivate.g(123);
  const bad_2 = ModuleWithPrivate.stuff;
*/
```

</Syntax>
