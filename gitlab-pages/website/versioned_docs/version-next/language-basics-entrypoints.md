---
id: version-next-language-basics-entrypoints
title: Entrypoints
original_id: language-basics-entrypoints
---

## Defining an entry point

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```Pascal
function main (const p : int ; const s : int) : (list(operation) * int) is
  block {skip} with ((nil : list(operation)), s + 1)
```
<!--END_DOCUSAURUS_CODE_TABS-->

## Multiple entry points

Multiple entrypoints are currently not supported in Michelson, however with Ligo, you can work that around by using variants.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```Pascal
// variant defining pseudo multi-entrypoint actions
type action is
| Increment of int
| Decrement of int

function add (const a : int ; const b : int) : int is
    block { skip } with a + b

function subtract (const a : int ; const b : int) : int is
    block { skip } with a - b

// real entrypoint that re-routes the flow based on the action provided
function main (const p : action ; const s : int) : (list(operation) * int) is
  block {skip} with ((nil : list(operation)),
    case p of
    | Increment n -> add(s, n)
    | Decrement n -> subtract(s, n)
    end)
```


<!--END_DOCUSAURUS_CODE_TABS-->
