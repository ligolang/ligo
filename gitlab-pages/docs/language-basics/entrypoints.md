---
id: entrypoints
title: Entrypoints
---

Entrypoints serve as a gate to our smart contracts. In LIGO each entrypoint is a function that accepts two arguments - first one is the parameter used to invoke the contract, and the second is the current storage of the contract. Each entrypoint has to return a list of operations to apply as a result of the smart contract call, and a new storage value.

> If you don't want to update the storage, don't worry, just re-cycle your last storage value.

## Defining an entry point

Contract below is effectively an empty contract, that takes a `unit` as a parameter, and returns a `unit` as well.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```Pascal
function main (const p : unit ; const s : unit) : (list(operation) * unit) is
  block {skip} with ((nil : list(operation)), s)
```
<!--END_DOCUSAURUS_CODE_TABS-->

## Multiple entry points

Multiple entrypoints are currently not supported in Michelson yet, however with Ligo, you can work that around by using variants & pattern matching.

In the example below we have a simple counter contract, that can be either `Increment(int)`-ed, or `Decrement(int)`-ed.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```Pascal
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
