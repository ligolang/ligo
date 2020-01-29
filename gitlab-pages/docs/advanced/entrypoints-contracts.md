---
id: entrypoints-contracts
title: Entrypoints, Contracts
---

## Entrypoints

Each LIGO smart contract is essentially a single main function, referring to the following types:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=a
type parameter_t is unit
type storage_t is unit
type return_t is (list(operation) * storage_t)
```

<!--CameLIGO-->
```cameligo group=a
type parameter_t = unit
type storage_t = unit
type return_t = (operation list * storage_t)
```

<!--ReasonLIGO-->
```reasonligo group=a
type parameter_t = unit;
type storage_t = unit;
type return_t = (list(operation) , storage_t);
```
<!--END_DOCUSAURUS_CODE_TABS-->

Each main function receives two arguments:
- `parameter` - this is the parameter received in the invocation operation
- `storage` - this is the current (real) on-chain storage value

Storage can only be modified by running the smart contract entrypoint, which is responsible for returning a pair holding a list of operations, and a new storage.

Here is an example of a smart contract main function:

> 💡 The contract below literally does *nothing*

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=a
function main(const parameter: parameter_t; const store: storage_t): return_t is
    ((nil : list(operation)), store)
```

<!--CameLIGO-->
```cameligo group=a
let main (parameter, store: parameter_t * storage_t) : return_t =
  (([]: operation list), store)
```

<!--ReasonLIGO-->
```reasonligo group=a
let main = ((parameter, store): (parameter_t, storage_t)) : return_t => {
  (([]: list(operation)), store);
};
```
<!--END_DOCUSAURUS_CODE_TABS-->

A contract entrypoints are the constructors of the parameter type (variant) and you must use pattern matching (`case`, `match`, `switch`) on the parameter in order to associate each entrypoint to its corresponding handler.

To access the 'entrypoints' of a contract, we define a main function whose parameter is a variant type with constructors for each entrypoint. This allows us to satisfy the requirement that LIGO contracts always begin execution from the same function. The main function simply takes this variant, pattern matches it to determine which entrypoint to dispatch the call to, then returns the result of executing that entrypoint with the projected arguments.

> The LIGO variant's are compiled to a Michelson annotated tree of union type.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=recordentry
type parameter_t is
  | Entrypoint_a of int
  | Entrypoint_b of string
type storage_t is unit
type return_t is (list(operation) * storage_t)

function handle_a (const p : int; const store : storage_t) : return_t is
  ((nil : list(operation)), store)

function handle_b (const p : string; const store : storage_t) : return_t is
  ((nil : list(operation)), store)

function main(const parameter: parameter_t; const store: storage_t): return_t is
  case parameter of
  | Entrypoint_a (p) -> handle_a(p,store)
  | Entrypoint_b (p) -> handle_b(p,store)
end
```

<!--CameLIGO-->
```cameligo group=recordentry
type parameter_t =
  | Entrypoint_a of int
  | Entrypoint_b of string
type storage_t = unit
type return_t = (operation list * storage_t)

let handle_a (parameter, store: int * storage_t) : return_t =
  (([]: operation list), store)

let handle_b (parameter, store: string * storage_t) : return_t =
  (([]: operation list), store)

let main (parameter, store: parameter_t * storage_t) : return_t =
  match parameter with
    | Entrypoint_a p -> handle_a (p,store)
    | Entrypoint_b p -> handle_b (p,store)
```

<!--ReasonLIGO-->
```reasonligo group=recordentry
type parameter_t =
  | Entrypoint_a(int)
  | Entrypoint_b(string);
type storage_t = unit;
type return_t = (list(operation) , storage_t);

let handle_a = ((parameter, store): (int, storage_t)) : return_t => {
  (([]: list(operation)), store); };

let handle_b = ((parameter, store): (string, storage_t)) : return_t => {
  (([]: list(operation)), store); };

let main = ((parameter, store): (parameter_t, storage_t)) : return_t => {
  switch (parameter) {
  | Entrypoint_a(p) => handle_a((p,store))
  | Entrypoint_b(p) => handle_b((p,store))
  }
};
```
<!--END_DOCUSAURUS_CODE_TABS-->


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

<!--CameLIGO-->
```cameligo group=b
let main (p, s: unit * unit) : operation list * unit =
  if amount > 0mutez
  then (failwith "This contract does not accept tez": operation list * unit)
  else (([]: operation list), unit)
```

<!--ReasonLIGO-->
```reasonligo group=b
let main = ((p,s): (unit, unit)) : (list(operation), unit) => {
  if (amount > 0mutez) {
    (failwith("This contract does not accept tez"): (list(operation), unit));
  }
  else {
    (([]: list(operation)), ());
  };
};
```

<!--END_DOCUSAURUS_CODE_TABS-->

### Access Control

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

<!--CameLIGO-->
```cameligo group=c
let owner: address = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address)
let main (p,s: unit * unit) : operation list * unit =
  if source <> owner
  then (failwith "This address can't call the contract": operation list * unit)
  else (([]: operation list), ())
```

<!--ReasonLIGO-->
```reasonligo group=c
let owner: address = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address);
let main = ((p,s): (unit, unit)) : (list(operation), unit) => {
  if (source != owner) {
    (failwith("This address can't call the contract"): (list(operation), unit));
  }
  else {
    (([]: list(operation)), ());
  };
};
```
<!--END_DOCUSAURUS_CODE_TABS-->

### Cross contract calls

This example shows how a contract can invoke another contract by emiting a transaction operation at the end of an entrypoint.

> The same technique can be used to transfer tez to an implicit account (tz1, ...), all you have to do is use `unit` instead of a parameter for a smart contract.

In our case, we have a `counter.ligo` contract that accepts a parameter of type `action`, and we have a `proxy.ligo` contract that accepts the same parameter type, and forwards the call to the deployed counter contract.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
// counter.ligo
type action is
| Increment of int
| Decrement of int
| Reset of unit

```

```pascaligo skip
// proxy.ligo

type action is
| Increment of int
| Decrement of int
| Reset of unit

const dest: address = ("KT19wgxcuXG9VH4Af5Tpm1vqEKdaMFpznXT3": address);

function proxy(const param: action; const store: unit): (list(operation) * unit)
    is block {
        const counter: contract(action) = get_contract(dest);
        // re-use the param passed to the proxy in the subsequent transaction
        // e.g.:
        // const mockParam: action = Increment(5);
        const op: operation = transaction(param, 0mutez, counter);
        const opList: list(operation) = list op; end;
    } with (opList, store)
```
<!--CameLIGO-->
```cameligo
// counter.mligo
type action = 
| Increment of int
| Decrement of int
| Reset of unit

// ...
```

```cameligo
// proxy.mligo

type action = 
| Increment of int
| Decrement of int
| Reset of unit

let dest: address = ("KT19wgxcuXG9VH4Af5Tpm1vqEKdaMFpznXT3": address)

let proxy (param, storage: action * unit): operation list * unit =
  let counter: action contract = Operation.get_contract dest in
  let op: operation = Operation.transaction param 0mutez counter in
  [op], storage
```

<!--ReasonLIGO-->
```reasonligo
// counter.religo

type action =
  | Increment(int)
  | Decrement(int)
  | Reset(unit);

// ...
```

```reasonligo
// proxy.religo

type action =
  | Increment(int)
  | Decrement(int)
  | Reset(unit);

let dest: address = ("KT19wgxcuXG9VH4Af5Tpm1vqEKdaMFpznXT3": address);

let proxy = ((param, s): (action, unit)): (list(operation), unit) =>
  let counter: contract(action) = Operation.get_contract(dest);
  let op: operation = Operation.transaction(param, 0mutez, counter);
  ([op], s);
```

<!--END_DOCUSAURUS_CODE_TABS-->
