---
id: tz-vs-eth
title: Migrating from Ethereum
---

import Syntax from '@theme/Syntax';


This article is aimed at those who have some experience with
developing smart contracts for Ethereum in Solidity. We will cover the
key differences between Solidity and LIGO, compare the execution model
of Ethereum and Tezos blockchains, and list the features you should be
aware of while developing smart contracts for Tezos.

## Languages and libraries

Tezos is an upgradeable blockchain that focuses on
decentralisation. It offers a wide variety of languages, frameworks,
and tools you can use to develop your contracts. In this article, we
mainly focus on the LIGO language, and the provided examples use the
Truffle framework for testing. However, many of the points here cover
the inherent differences in the blockchain architectures, so they
should be valid for other languages and frameworks in the Tezos
ecosystem.

The current Tezos protocol uses the Michelson language under the
hood. Michelson is much like EVM in Ethereum, inasmuch as its programs
are low-level code executed by an embedded virtual
machine. Nevertheless, contrary to EVM byte-code, Michelson is a
strongly-typed stack-based language designed to be human-readable.

Having a human-readable representation of compiled contracts makes it
harder for compiler bugs to pass unnoticed: everyone can review the
Michelson code of the contract and even formally prove its
correctness.

LIGO is a family of high-level languages. There are several _flavours_
or _syntaxes_ of LIGO – CameLIGO, and JsLIGO. The developers may
choose whatever syntax looks more familiar to them.

## Terminology

For those who come from the Ethereum world, the terminology used in
Tezos may be misleading. Tezos developers chose to _not_ reuse the
same terms for similar concepts for a reason: a false sense of
similarity would be a bad friend for those migrating to a different
blockchain architecture.

We will, however, try to associate the terms known to you with the
terms used in Tezos. Note that this is just an _association,_ and not
the exact equivalence.

| Ethereum term            | Tezos term           | Notes |
|--------------------------|----------------------|-------|
| World state              | Context              | |
| Account                  | Contract or Account  | In Tezos, both smart contracts and accounts controlled by private keys are referred to as "contracts" |
| Externally-owned account | Implicit account     | |
| Contract                 | Smart contract or Originated contract  | |
| Contract deployment      | Contract origination | |
| Transaction              | Operation            | In Tezos, there is a distinction between transactions that transfer value, contract originations, and other kinds of operations |
| –                        | Transaction          | One possible type of operation (value transfer or contract invocation) |
| Miner                    | Baker                | Tezos uses proof-of-stake, so bakers do not solve proof-of-work puzzles. They do produce new blocks and receive rewards, though |
| Contract state           | Contract storage     | |
| Contract method          | Entrypoint           | |
| View method              | – | Currently, Tezos does not provide view functions. You can inspect the storage of the contract, though |

## Types and why they matter

If you come from the Solidity world, you may be accustomed to simple types like `string` or `uint256`, structures, and enums. In LIGO, the types are more advanced. They tend to reflect how the values of these types should be used rather than how they are stored. That is why, for example, LIGO has separate types for `signature` and public `key` instead of just using a byte string (`bytes`) everywhere. Numeric types follow this philosophy: the numbers have arbitrary precision and are, in practice, bounded only by the transaction gas consumption.

Since types are not some purely "technical" concept and have inherent meaning attached to them, it is often a good idea to start thinking about your contract in terms of functions that transform values of one type to values of, possibly, some other type.

You can define new types and type aliases in your code using the `type` keyword:

<Syntax syntax="cameligo">

```cameligo group=b2
type nat_alias = nat

type token_amount = TokenAmount of nat
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=b2
type nat_alias = nat;

type token_amount = | ["TokenAmount", nat];
```

</Syntax>

As in Solidity, there are record types:

<Syntax syntax="cameligo">

```cameligo group=b3
type creature = {heads_count : nat; legs_count : nat; tails_count : nat}
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=b3
type creature = { heads_count: nat, legs_count: nat, tails_count: nat };
```

</Syntax>


There are also _variant_ types (or "sum types") – a more powerful counterpart of Solidity enums that can hold data:

<Syntax syntax="cameligo">

```cameligo group=b4
type int_option = Number of int | Null

let x = Number 5

let y = Null
```

Valid values of this type are regular numbers wrapped in `Number` (e.g., `Number 5`, `Number 10`, etc.) or `Null`. Notice how `Null` does not hold any value.

</Syntax>



<Syntax syntax="jsligo">

```jsligo group=b4
type int_option = ["Number", int] | ["Null"];

let x = Number(5);

let y = Null ();
```

Valid values of this type are regular numbers wrapped in `Number` (e.g., `Number(5)`, `Number(10)`, etc.) or `Null`. Notice how `Null()` does not hold any value.

</Syntax>


There is a special built-in parameterised `option` type with `Some` and `None` constructors, so we can rewrite the snippet above as:

<Syntax syntax="cameligo">

```cameligo group=b5
let x = Some 5

let y : int option = None
```

This is how we express _nullability_ in LIGO: instead of using a special ad-hoc value like "zero address", we just say it is an `address option`. We can then use `match` to see if there is something inside:

```cameligo group=b6
let x : int option = Some 5

let x_or_zero =
  match x with
    Some value -> value
  | None -> 0
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=b5
let x = Some(5);

let y: option<int> = None();
```

This is how we express _nullability_ in LIGO: instead of using a special ad-hoc value like "zero address", we just say it is an `option<address>`. We can then use `match` to see if there is something inside:

```jsligo group=b6
let x = Some (5);

let x_or_zero  =
  match(x) {
    when(Some(value)): value;
    when(None()): 0
  };
```

</Syntax>


We can go further and combine variant types with records:

<Syntax syntax="cameligo">

```cameligo group=b7
type committee = {members : address list; quorum : nat}

type leader = {name : string; address : address}

type authority = Dictatorship of leader | Democracy of committee
```

</Syntax>



<Syntax syntax="jsligo">

```jsligo group=b7
type committee = {members: list<address>, quorum: nat };

type leader = {name: string, address: address };

type authority = ["Dictatorship", leader] | ["Democracy", committee];
```

</Syntax>


## Contracts and entrypoints

In Solidity, you usually define a _contract_ with _methods_ and _fields_:

```solidity
contract Counter {
  int public counter;  // storage field

  function increment() public { ... } // method
  function decrement() public { ... } // method
}
```

When the contract is compiled, the Solidity compiler automatically adds dispatching logic into the resulting EVM byte-code. The contract inspects the data passed to it and chooses a method based on the first four bytes of the method's signature hash: `0xbc1ecb8e` means `increment()` and `0x36e44653` means `decrement()`.

In Tezos, a contract must define a default entrypoint that does the dispatching. It is much like a `main` function in C-like languages. It accepts a typed _parameter_ (some data that comes with a transaction) and the current value of the contract _storage_ (the internal state of the contract). The default entrypoint returns a list of internal operations and the new value of the storage.

For example, we can simulate Ethereum dispatching behaviour:

<Syntax syntax="cameligo">

```cameligo group=a2
[@entry]
let main (parameter : bytes) (storage : int) : operation list * int =
  if parameter = 0xbc1ecb8e
  then [], storage + 1
  else
    if parameter = 0x36e44653
    then [], storage - 1
    else failwith "Unknown entrypoint"
```

However, we can do better. As we discussed, LIGO has a much richer type system than Solidity does. We can encode the entrypoints by marking the functions which are entry points. For our counter contract, we can say, e.g., that the entry points are _either_ `Increment` or `Decrement`, and implement their behaviours as separate functions:

```cameligo group=a3
type storage = int
type result = operation list * storage

[@entry]
let increment (_ : unit) (s : storage) : result = [], s + 1

[@entry]
let decrement (_ : unit) (s : storage) : result = [], s - 1
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=a2
let main = (parameter: bytes, storage: int): [list<operation>, int] => {
  if (parameter == 0xbc1ecb8e) {
    return [list([]), storage + 1]
  } else {
    if (parameter == 0x36e44653) {
      return [list([]), storage - 1]
    } else {
      return (failwith("Unknown entrypoint"))
    }
  }
};
```

However, we can do better. As we discussed, LIGO has a much richer type system than Solidity does. We can encode the entrypoint directly in the parameter type. that the entry points are _either_ `Increment` or `Decrement`, and implement their behaviours as separate functions:

```jsligo group=a3
type storage = int;
type result = [list<operation>, int]

@entry
const increment = (_u : unit, s : storage) : result => [list([]), s + 1]

@entry
const decrement = (_u : unit, s : storage) : result => [list([]), s - 1]
```

</Syntax>


We do not need any internal operations, since we neither call other contracts nor transfer money. Here is how we can add arguments to our entrypoints:

<Syntax syntax="cameligo">

```cameligo group=a4
type storage = int
type result = operation list * storage

[@entry]
let add (i : int) (s : storage) : result = [], s + i

[@entry]
let subtract (i : int) (s : storage) : result = [], s - i
```

</Syntax>


<Syntax syntax="jsligo">

```jsligo group=a4
type storage = int;
type result = [list<operation>, int]

@entry
const add = (i : int, s : storage) : result => [list([]), s + i]

@entry
const subtract = (i : int, s : storage) : result => [list([]), s - i]
```

</Syntax>


Tezos has special support for parameters encoded with variant types. If the parameter is a variant type, Tezos will treat each constructor as a separate entrypoint (with the first letter lowercased). It is important when we want to call a contract but do not know the full type of its parameter. For example, we can call our counter contract with the following CLI command:

`tezos-client call contract counter from alice --entrypoint '%subtract' --arg 100`

Truffle (and Taquito library, which Truffle for Tezos uses under the hood), also treats entrypoints specially. We can call our `add` entrypoint as follows:
```solidity
const Counter = artifacts.require('Counter')
let counterInstance = await Counter.deployed()
await counterInstance.add(100)
```

## Visibility modifiers

Solidity has visibility modifiers like `private` and `public` for storage entries and contract methods. LIGO has none of these, and you may be wondering why. To answer this, we will first consider storage modifiers and then discuss methods (entrypoints).

It is a popular misconception in the Ethereum world that by marking a storage field `private` you can make this field visible only from inside the contract. Both in Tezos and Ethereum, the contract storage is public. This is due to how blockchains work: nodes need to read contracts' storage to execute and validate the transactions. Tezos allows anyone to inspect storage of any contract with one CLI command. In Ethereum, it is harder but still feasible.

Making a storage field `public` in Solidity instructs the compiler to generate a `view` method that returns the value of this field. In Tezos, it is possible to inspect the storage directly, and it is not possible to return values from contract calls. Thus, public and private storage fields are equivalent in Tezos.

For contract methods, the dispatching logic defines which functions within the contract are accessible from the outside world. Consider the following snippet:

<Syntax syntax="cameligo">

```cameligo group=a5
type storage = int
type result = operation list * storage

let doMultiplyBy2 (store : storage) : int = store * 2

let doMultiplyBy4 (store : storage) : int = doMultiplyBy2 (doMultiplyBy2 store)

[@entry] let multiplyBy4 (_ : unit) (s : storage) : result = [], doMultiplyBy4 s
[@entry] let multiplyBy16 (_ : unit) (s : storage) : result = [], doMultiplyBy4 (doMultiplyBy4 s)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=a5
type storage = int
type result = [list<operation>, storage]

let doMultiplyBy2 = (store : storage) : int => store * 2;

let doMultiplyBy4 = (store : storage) : int => doMultiplyBy2(doMultiplyBy2(store));

@entry const multiplyBy4 = (_u : unit, s : storage) : result => [list([]), doMultiplyBy4(s)]
@entry const multiplyBy16 = (_u : unit, s : storage) : result => [list([]), doMultiplyBy4(doMultiplyBy4(s))]
```

</Syntax>

Here:
1. `multiplyBy2` is _private_ (in Solidity terms): we cannot call it directly from outside of the contract.
2. `multiplyBy4` is _public:_ we can call it both from inside the contract and using the `%multiplyBy4` entrypoint.
3. `%multiplyBy16` is _external:_ there is no function `multiplyBy16` in the contract so we cannot call it from inside the source code, but there is an entrypoint `%multiplyBy16` encoded in the parameter, so we can use tezos-client or Taquito to call it externally.

There is no analogue of `internal` methods in LIGO because LIGO contracts do not support inheritance.

## Lambdas

In Tezos, you can accept _code_ as a parameter. Such functions that you can pass around are called _lambdas_ in functional languages. Let us say that we want to support arbitrary mathematical operations with the counter value. We can just accept the intended formula as the parameter:

<Syntax syntax="cameligo">

```cameligo group=a6

type storage = int

[@entry]
let compute (func : int -> int) (s : storage) : operation list * storage =
  [], func s
```

We can then call this contract with the parameter of the form `Compute (fun (x : int) -> x * x + 2 * x + 1)`. Try this out with:
```
ligo run interpret 'main (Compute (fun (x : int) -> x * x + 2 * x + 1), 3)' --init-file examples/contracts/mligo/Lambda.mligo
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=a6
type storage = int;

@entry
const compute = (func: ((v : int) => int), s: storage) : [list<operation>, int] =>
  [list([]), func(s)]
```

We can then call this contract with the parameter of the form `Compute ((x : int) => x * x + 2 * x + 1)`. Try this out with:
```
ligo run interpret 'main([Compute ((x : int) => x * x + 2 * x + 1), 3])' --init-file examples/contracts/jsligo/Lambda.jsligo
```

</Syntax>

The interpreted output is `( LIST_EMPTY() , 16 )`, which is an empty list of operations and the new storage value – the result of the computation.

But this is not all lambdas are capable of. You can, for example, save them in storage:

<Syntax syntax="cameligo">

```cameligo group=a1
type storage = {fn : (int -> int) option; value : int}
type result = operation list * storage

let call (fn : (int -> int) option) (value : int) =
  match fn with
    Some f -> f value
  | None -> failwith "Lambda is not set"

[@entry]
let setFunction (fn : int -> int) (s : storage) : result =
  [], { s with fn = Some fn }

[@entry]
let callFunction (_ : unit) (s : storage) : result =
  [], { s with value = call s.fn s.value }
```

Now we can _upgrade_ a part of the implementation by calling our contract with `SetFunction (fun (x : int) -> ...)`.

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=a1
type storage = { fn : option<((x : int) => int)>, value : int };
type result = [list<operation>, storage];

const call = (fn: option<((x : int) => int)>, value: int) : int => {
  return match(fn) {
    when(Some(f)): f(value);
    when(None()): failwith("Lambda is not set")
  }
};

@entry
const setFunction = (fn : ((v : int) => int), s : storage) : result =>
  [list([]), {...s, fn: Some(fn)}];

@entry
const callFunction = (_u : unit, s : storage) : result =>
  [list([]), {...s, value: call(s.fn, s.value)}];
```

Now we can _upgrade_ a part of the implementation by calling our contract with `SetFunction ((x : int) => ...)`.

</Syntax>

## Execution model

In Ethereum, you often find yourself "calling" other contracts and splitting your business logic into multiple independent parts. When you call some other contract, the transaction execution is paused until the callee returns the result. We will refer to such invocations as _direct calls:_

```solidity
contract Treasury {
    uint256 public rewardsLeft;
    IBeneficiary beneficiary;

    function disburseRewards() public {
        require(rewardsLeft != 0);
        beneficiary.handleRewards().call.value(rewardsLeft);
        // the execution is paused until `handleRewards` returns
        rewardsLeft = 0;
    }
}
```

Those of you experienced with Solidity may notice that this contract is not reentrancy-safe: the beneficiary contract may utilise the fact that by the time of the call, `rewardsLeft` storage variable has not been updated. The attacker can _call back_ into the caller contract, invoking `disburseRewards` until it drains the treasury contract:

```solidity
contract Beneficiary {
    function handleRewards() public payable {
        Treasury treasury = Treasury(msg.sender);
        if (msg.sender.balance > treasury.rewardsLeft) {
            treasury.disburseRewards();
        }
    }
}
```

In Tezos, the execution model is quite different. Contracts communicate via message passing. Messages are called _internal operations._ If you want to pass a message to another contract, you need to finish the computation first, and then put an operation into the _operations queue._ Here is how it looks like:

<Syntax syntax="cameligo">

```cameligo group=b1
type storage = {rewardsLeft : tez; beneficiaryAddress : address}

let treasury (p, s : unit * storage) =
  // We do our computations first
  let newStorage = {s with rewardsLeft = 0mutez} in

  // Then we find our beneficiary's `handleRewards` entrypoint:
  let beneficiaryOpt = Tezos.get_entrypoint_opt "%handleTransfer" s.beneficiaryAddress in
  let beneficiary =
    match beneficiaryOpt with
      Some contract -> contract
    | None -> failwith "Beneficiary does not exist" in

  // Then we prepare the internal operation we want to perform
  let operation = Tezos.transaction () s.rewardsLeft beneficiary in

  // ...and return both the operations and the updated storage
  ([operation], newStorage)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=b1
type storage = {rewardsLeft: tez, beneficiaryAddress: address };

let treasury = (p : unit, s : storage) => {
  // We do our computations first
  let newStorage = {...s, rewardsLeft: 0mutez};

  // Then we find our beneficiary's `handleRewards` entrypoint:
  let beneficiaryOpt = Tezos.get_entrypoint_opt("%handleTransfer", s.beneficiaryAddress);
  let beneficiary =
    match(beneficiaryOpt) {
     when(Some(contract)): contract;
     when(None()): failwith("Beneficiary does not exist")
    };

  // Then we prepare the internal operation we want to perform
  let operation = Tezos.transaction(unit, s.rewardsLeft, beneficiary);

  // ...and return both the operations and the updated storage
  return [list([operation]), newStorage];
};
```

</Syntax>

Note that all the state changes occur _before_ the internal operation gets executed. This way, Tezos protects us from unintended reentrancy attacks. However, with complex interactions chain, reentrancy attacks may still be possible.

It is a common idiom in Ethereum to make read-only calls to other contracts. Tezos does not offer a straightforward way to do it but you might think of using something like a callback mechanism:

<Syntax syntax="cameligo">

```cameligo skip
type parameter = DoSomething | DoSomethingCont of int

let doSomething (p, s : unit * int) =
  (* The callee should call `%doSomethingCont` with the value we want *)
  let op = Tezos.transaction ... in
  ([op], s)

let doSomethingCont (p, s : int * int) = ([] : operation list), p + s
```

</Syntax>

<Syntax>

```jsligo skip
type parameter = ["DoSomething"] | ["DoSomethingCont", int];

let doSomething = ([p, s]: [unit, int]) => {
  /* The callee should call `%doSomethingCont` with the value we want */
  let op = Tezos.transaction ...;
  return [list([op]), s]
}

let doSomethingCont = ([p, s]: [int, int]) => [(list([]) as list<operation>), p + s];
```

</Syntax>

However, here you leave your contract in an _intermediate_ state before making an external call. You would need additional precautions to make such callback-style calls secure. In most cases, you should avoid this pattern.

By making contract interactions harder, Tezos incentives you to simplify your architecture. Think about whether you can use lambdas or merge your contracts to avoid complex inter-contract dependencies. If it is possible to _not_ split your logic into multiple contracts, then avoid the split.

You can find more details on how Tezos contracts interact with each other in our [inter-contract calls](../inter-contract-calls/inter-contract-calls.md) article.

## Fees

Fee model in Tezos is more complicated than the Ethereum one. The most important bits you should know about are:
1. In Tezos, you _burn_ a certain amount of Tez for increasing the size of the stored data. For example, if you add a new entry to a map or replace a string with a longer one, you must burn your Tez tokens.
2. When you call a contract, the transaction spends gas for reading, deserialising and type-checking the storage. Also, a certain amount of gas gets spent for serialising and writing the storage back to the context. In practice, it means that **the larger your code and storage are, the more expensive it is to call your contract,** regardless of the number of computations performed. If you have big or unbounded containers in storage, you should most probably use `big_map`.
3. Emitting internal operations is very expensive in terms of gas: there is a fixed cost of 10000 gas for `Tezos.get_{contract, entrypoint}_opt` plus the cost of reading, deserialising, and type-checking the parameter of the callee.

Always test for gas consumption and strive to minimise the size of the data stored on chain and the number of internal operations emitted. You can read more on fees in our [Optimisation guide](../optimisation/optimisation.md) or in the [Serokell blog post](https://medium.com/tqtezos/how-to-minimize-transaction-costs-of-tezos-smart-contracts-9962347faf64).

## Conclusion

In this article, we discussed some Solidity patterns and their LIGO counterparts. We also covered the most important aspects of the Tezos execution model and fees. Here is a quick reference table comparing Solidity and LIGO patterns:

| Solidity pattern | LIGO pattern |
|------------------|--------------|
| `public` field   | A field in the storage record, e.g. <Syntax syntax="cameligo">`type storage = { x : int; y : nat }`</Syntax><Syntax syntax="jsligo">`type storage = { x : int, y : nat }`</Syntax> |
| `private` field  | N/A: all fields are public |
| `private` method | A regular function, e.g., <Syntax syntax="cameligo">`let func (a : int) = ...`</Syntax><Syntax syntax="jsligo">`let func = (a : int) => ...`</Syntax> |
| `public` /  `external` method  | A separate entrypoint in the parameter: <Syntax syntax="cameligo">`type parameter = F of int`</Syntax><Syntax syntax="jsligo">`type parameter = ["F", int]`</Syntax>. `main` entrypoint should dispatch and forward this call to the corresponding function using a match expression |
| `internal` method | There is no concept of inheritance in Tezos |
| Constructor      | Set the initial storage upon origination |
| Method that returns a value | Inspect the contract storage directly |
| `contract.doX(...)` | Emit an internal operation |
| `uint x = contract.getX()` | Do not do this. Think if you can merge the contracts or reverse the execution flow |
| Proxy upgrade pattern | Put lambdas to storage and provide means to update them |
| `emit Event(...)` | Event logs are not supported at the moment. There is a [proposal](https://tezos.gitlab.io/protocols/014_kathmandu.html#contract-event-logging) to support event logs in the future |
