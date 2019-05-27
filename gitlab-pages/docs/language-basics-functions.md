---
id: language-basics-functions
title: Functions
---

## Defining a function

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```Pascal
// multiply(1, 2) = 2
function multiply (const a : int ; const b : int) : int is
    begin
        const result : int = a * b ;
    end with result

// add(1, 2) = 3
function add (const a : int ; const b : int) : int is
    block { skip } with a + b
```

<!--END_DOCUSAURUS_CODE_TABS-->