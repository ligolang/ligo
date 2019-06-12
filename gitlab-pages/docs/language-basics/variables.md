---
id: variables
title: Variables
---

## Defining a variable

Variables in LIGO can be defined in two ways - by using either the `const` or `var` keywords. `const` can be used both at global (top-level) and local scope (within functions/blocks), while `var` can be used for mutable values in the local scope.


### Imutable variables using `const`

> ⚠️ Currently const values are mutable as well, however this is something that will change in the upcoming release. For the time being think of `const` as a semantical way to indicate developer intentions.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```Pascal
const four: int = 4;
```

<!--END_DOCUSAURUS_CODE_TABS-->

### Mutable variables using `var` 

> ⚠️ `var` can't be used in the global scope

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```Pascal
// won't work, use const for global values instead
var four: int = 4;

// value of `number` can be mutated within local scope
function addFour(var number: int): int is
  block { 
    number := number + 4;
  } with number;
```

<!--END_DOCUSAURUS_CODE_TABS-->