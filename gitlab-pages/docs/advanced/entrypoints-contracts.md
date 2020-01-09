---
id: entrypoints-contracts
title: Entrypoints, Contracts
---

## Entrypoints

Each LIGO smart contract is essentially a single function, that has the following *(pseudo)* type signature:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```
(const parameter: my_type, const store: my_store_type): (list(operation), my_store_type)
```
<!--END_DOCUSAURUS_CODE_TABS-->

This means that every smart contract needs at least one entrypoint function, here's an example:

> 💡 The contract below literally does *nothing*

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=a
type parameter is unit;
type store is unit;
function main(const parameter: parameter; const store: store): (list(operation) * store) is
    block { skip } with ((nil : list(operation)), store)
```
<!--END_DOCUSAURUS_CODE_TABS-->

Each entrypoint function receives two arguments:
- `parameter` - this is the parameter received in the invocation operation
- `storage` - this is the current (real) on-chain storage value

Storage can only be modified by running the smart contract entrypoint, which is responsible for returning a list of operations, and a new storage at the end of it's execution.


## Built-in contract variables

Each LIGO smart contract deployed on the Tezos blockchain, has access to certain built-in variables/constants that can be used to determine a range
of useful things. In this section you'll find how those built-ins can be utilized.

### Accepting/declining money in a smart contract

This example shows how `amount` and `failwith` can be used to decline a transaction that sends more tez than `0mutez`.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=b
function main (const p : unit ; const s : unit) : (list(operation) * unit) is
  block {
      if amount > 0mutez then failwith("This contract does not accept tez") else skip
  } with ((nil : list(operation)), unit);
```
<!--END_DOCUSAURUS_CODE_TABS-->

### Access control locking

This example shows how `sender` or `source` can be used to deny access to an entrypoint.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=c
const owner: address = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address);
function main (const p : unit ; const s : unit) : (list(operation) * unit) is
  block {
      if source =/= owner then failwith("This address can't call the contract") else skip
  } with ((nil : list(operation)), unit);
```
<!--END_DOCUSAURUS_CODE_TABS-->

### Cross contract calls

This example shows how a contract can invoke another contract by emiting a transaction operation at the end of an entrypoint.

> The same technique can be used to transfer tez to an implicit account (tz1, ...), all you have to do is use `unit` instead of a parameter for a smart contract.

In our case, we have a `counter.ligo` contract that accepts a parameter of type `action`, and we have a `proxy.ligo` contract that accepts the same parameter type, and forwards the call to the deployed counter contract.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=dup
// counter.types.ligo
type action is
| Increment of int
| Decrement of int
| Reset of unit
```

```pascaligo group=d
// counter.ligo
type action is
| Increment of int
| Decrement of int
| Reset of unit
```

```pascaligo skip
// proxy.ligo
#include "counter.types.ligo"

const address: address = ("KT19wgxcuXG9VH4Af5Tpm1vqEKdaMFpznXT3": address);

function proxy(const param: action; const store: unit): (list(operation) * unit)
    is block {
        const counter: contract(action) = get_contract(address);
        // re-use the param passed to the proxy in the subsequent transaction
        // e.g.:
        // const mockParam: action = Increment(5);
        const op: operation = transaction(param, 0mutez, counter);
        const opList: list(operation) = list op; end;
    } with (opList, store)
```
<!--END_DOCUSAURUS_CODE_TABS-->