---
id: entrypoints
title: Entrypoints
---

Entrypoints are the gates to a smart contract. In LIGO each entrypoint is a function that accepts two arguments. The first is the parameter used to invoke the contract, and the second is the current storage of the contract. Each entrypoint must return a list of operations to apply as a result of the smart contract call, and a new storage value.

> If you don't want to update the storage, don't worry, just re-cycle your last storage value.

## Defining an entry point

The contract below is effectively an empty contract. It takes a `unit` as a parameter, and returns a `unit`.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
function main (const p : unit ; const s : unit) : (list(operation) * unit) is
  block {skip} with ((nil : list(operation)), s)
```
<!--END_DOCUSAURUS_CODE_TABS-->

## Multiple entry points

Multiple entrypoints are currently not supported in Michelson. But with Ligo you can work around that by using variants & pattern matching.

In the example below we have a simple counter contract, that can be either `Increment(int)`-ed, or `Decrement(int)`-ed.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
// variant defining pseudo multi-entrypoint actions
type action is
| Increment of int
| Decrement of int

// real entrypoint that re-routes the flow based on the action (parameter) provided
function main (const action: action ; const counter: int) : (list(operation) * int) is
  block {skip} with ((nil : list(operation)),
    case action of
    | Increment(number) -> counter + number
    | Decrement(number) -> counter - number
    end)
```


<!--END_DOCUSAURUS_CODE_TABS-->
