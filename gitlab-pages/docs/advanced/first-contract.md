---
id: first-contract
title: First contract
---

So far so good, we've learned enough of the LIGO language, we're confident enough to write out first smart contract.

We'll be implementing a counter contract, let's go.

## Dry-running a contract

Testing a contract can be quite easy if we utilize LIGO's built-in dry run feature. Dry-run works by simulating the entrypoint execution, as if it were deployed on a real chain. You need to provide the following:

- `file` - contract to run
- `entrypoint` - name of the function to execute
- `parameter` - parameter passed to the entrypoint (in a theoretical invocation operation)
- `storage` - a mock storage value, as if it were stored on a real chain

Here's a full example:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```
ligo dry-run src/basic.ligo main Unit Unit
// Outputs:
// tuple[   list[]
//          Unit
// ]
```
<!--END_DOCUSAURUS_CODE_TABS-->

Output of the `dry-run` is the return value of our entrypoint function, we can see the operations emited - in our case an empty list, and the new storage value being returned - which in our case is still `Unit`.

## Building a counter contract

Our counter contract will store a single `int` in its storage, and will accept an `action` variant in order to re-route our single `main` entrypoint into two entrypoints for `addition` and `subtraction`. 

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```
type action is
| Increment of int
| Decrement of int

function main (const p : action ; const s : int) : (list(operation) * int) is
  block {skip} with ((nil : list(operation)),
    case p of
    | Increment (n) -> s + n
    | Decrement (n) -> s - n
    end)
```

<!--CameLIGO-->
```cameligo
type action =
| Increment of int
| Decrement of int

let main (p, s: action * int) : operation list * int =
  let result =
    match p with
    | Increment n -> s + n
    | Decrement n -> s - n
  in
  (([]: operation list), result)
```

<!--ReasonLIGO-->
```reasonligo
type action =
| Increment(int)
| Decrement(int);

let main = (p_s: (action, int)) : (list(operation), int) => {
  let p, s = p_s;
  let result =
    switch (p) {
    | Increment(n) => s + n
    | Decrement(n) => s - n
    };
  (([]: list(operation)), result);
};
```

<!--END_DOCUSAURUS_CODE_TABS-->

To dry-run the counter contract, we will use the `main` entrypoint, provide a variant parameter of `Increment(5)` and an initial storage value of `5`.


<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```
ligo dry-run src/counter.ligo main "Increment(5)" 5
// tuple[   list[]
//          10
// ]
```
<!--END_DOCUSAURUS_CODE_TABS-->


Yay, our contract's storage has been successfuly incremented to `10`.

## Deploying and interacting with a contract on a live-chain

In order to deploy the counter contract to a real Tezos network, we'd have to compile it first, this can be done with the help of the `compile-contract` CLI command:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```
ligo compile-contract src/counter.ligo main
```
<!--END_DOCUSAURUS_CODE_TABS-->


Command above will output the following Michelson code:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```
{ parameter (or (int %decrement) (int %increment)) ;
  storage int ;
  code { DUP ;
         CAR ;
         DIP { DUP } ;
         SWAP ;
         CDR ;
         DIP { DUP } ;
         SWAP ;
         IF_LEFT
           { DUP ;
             DIP 2 { DUP } ;
             DIG 2 ;
             DIP { DUP } ;
             SUB ;
             SWAP ;
             DROP ;
             SWAP ;
             DROP }
           { DUP ;
             DIP 2 { DUP } ;
             DIG 2 ;
             DIP { DUP } ;
             ADD ;
             SWAP ;
             DROP ;
             SWAP ;
             DROP } ;
         NIL operation ;
         PAIR ;
         SWAP ;
         DROP ;
         SWAP ;
         DROP ;
         SWAP ;
         DROP } }
```
<!--END_DOCUSAURUS_CODE_TABS-->

However in order to originate a Michelson contract on Tezos, we also need to provide the initial storage value, we can use `compile-storage` to compile the LIGO representation of the storage to Michelson.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```
ligo compile-storage src/counter.ligo main 5
// Outputs: 5
```
<!--END_DOCUSAURUS_CODE_TABS-->


In our case the LIGO storage value maps 1:1 to its Michelson representation, however this will not be the case once the parameter is of a more complex data type, like a record.

## Invoking a LIGO contract

Same rules apply for parameters, as apply for translating LIGO storage values to Michelson. We will need to use `compile-parameter` to compile our `action` variant into Michelson, here's how:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```
ligo compile-parameter src/counter.ligo main 'Increment(5)'
// Outputs: (Right 5)
```
<!--END_DOCUSAURUS_CODE_TABS-->


Now we can use `(Right 5)` which is a Michelson value, to invoke our contract - e.g. via `tezos-client`
