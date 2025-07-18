---
id: optimisation
title: Optimisation
---

import Syntax from '@theme/Syntax';

Optimisation is important in blockchain development because calls to smart contracts incur transaction fees in proportion to the amount of storage and computation that they use.
There is also a limit to the size of a smart contract's code.
Also, as described in [Security](./security), optimisation is important because there is a limit to the storage and computation that a single transaction can use (the _gas limit_), and therefore contracts that require too much storage or computation can become permanently unusable.

To be able to optimise your contracts and be sure they will be usable in the long term, you must understand what the limits of smart contracts are and how their storage and computation are calculated.
This page covers the limits of Tezos smart contracts, its fee model, and the basics of measuring and optimising contracts.

## Fees

Tezos transaction fees (commonly called _gas fees_) include two components:

- The execution fee includes a base fee and a variable fee that depends on the amount of computation that a transaction requires.
This processing includes loading data from storage, running the code of the contract, and storing updated data.
This fee is given to the baker as an incentive for baking.

   Bakers usually include transactions with the highest execution fees first, so you can manually increase the execution fee included with the transaction to make it more likely that bakers will include it sooner.
   Most tools add a small amount of tez to the execution fee as a buffer to ensure that it will be included.

- The storage fee (also known as the burn fee) depends on the increase in the amount of storage that a transaction causes.
This fee is burnt (destroyed) to prevent bloat.
It is not refunded when a smart contract's storage decreases.

   Callers pay a storage fee only when the new contract storage size exceeds the largest historical storage size.
   For example, assume this contract and its transactions:

   1. The contract is originated with storage that takes up 500 bytes.
   1. Transaction A reduces its storage to 400 bytes.
   1. Transaction B increases its storage to 450 bytes.
   1. Transaction C increases its storage to 505 bytes.

   In this case:

   - The storage fees for transaction A and B are zero because they do not increase the storage size beyond the historical maximum of 500 bytes.
   - The storage fee for transaction C is based on the cost of storing 5 bytes because that is how much it increases the storage past the historical maximum.

For information about calculating the actual fees for operations, see the Octez and protocol documentation at https://octez.tezos.com.

### Fees for originating a contract

When you originate (deploy) a contract, its code and initial storage are written to the blockchain.
No computation is involved, so you pay only the storage fee and the base amount of the execution fee.
The storage fee is the cost per byte multiplied by the sum of the bytes used in the contract code, the initial storage, and some protocol-specific data of constant size.

Therefore, to reduce the fees for originating a contract, you can reduce the size of the contract code and the size of the initial storage.

### Fees for calling a contract

When you call a contract, you pay an execution fee in proportion to the amount of computation required and a storage fee in proportion the amount of increased storage.
The computation includes deserialising the data in storage and the smart contract code, running the code, and serialising data in the updated storage.

Therefore, to reduce the fees for calling a contract:

- Keep the cost of deserialising its storage down by limiting the data it stores and by using lazily-deserialized types like big-maps.
- Keep the storage fees down by keeping the growth of its storage down.
- Reduce the size of the contract code.
- Make the contract code as efficient as possible.

## Limits

Tezos imposes limits to ensure that operations can be distributed and processed quickly to keep the network running smoothly.
Operations that exceed these limits are rejected regardless of the fees that you include with them:

- Contract origination is limited by the maximum size of a smart contract, and because contract origination is an operation, by the maximum size of an operation.

- Transactions that call contracts are limited by the maximum size of the operation itself (including the parameter passed to the contract) and by the maximum computation that an operation can use (the gas limit).

## Optimisation targets

For the reasons listed above, smart contract developers can focus on these optimisation targets:

1. Gas consumption
2. Contract code size
3. Total storage size
4. Amount of extra bytes written to storage

Another two factors – parameter size and operations pressure – are mostly out of control of the contract author.

Although the optimisation targets listed above are inter-related, you can look at them in isolation because the optimisation methods may differ.

### Gas consumption

Contrary to a more conventional instruction-based gas accounting, where each instruction has a cost associated with it, Tezos gas fees reflect actual computations and I/O operations performed by the nodes.
On one hand, this gas cost accounting prevents vulnerabilities caused by incorrect estimation of the instruction costs.
On the other hand, it makes the gas model more complex than, for example, the Ethereum model.

To understand how gas is spent, consider the phases of transaction execution:

1. Reading the serialised contract code and non-lazy storage from the context
2. Deserialising the bytes into an intermediate representation
3. Converting untyped intermediate representation of code and storage into typed values
4. Running the contract code
5. Serialising the result of the computation
6. Writing the updated storage bytes back to the context

At each phase, a certain amount of gas is consumed.

* The amount of gas consumed in phases 1–3 is proportional the size of the code and the size of the non-lazy storage.
* The cost of running the contract code (phase 4) depends on the number of instructions and the complexity of those instructions.

  - You can estimate the cost of simple, atomic instructions like variable assignments and comparisons based on the average gas cost of a Michelson instruction.

  - The cost of more complex, expensive instructions such as `Tezos.get_contract_opt` and `Bytes.pack` can be estimated separately.

* The amount of gas consumed in phases 5–6 is proportional to the size of the storage.

These are only approximations; for example, the true cost of deserialization also depends on the inherent complexity of the code and data types, but the primary variable is the size.
Similarly, not all simple, atomic instructions cost the same.
For detailed info on gas consumption, please refer to the [Tezos gas model description](https://gitlab.com/tezos/tezos/-/blob/52a074ab3eb43ad0087804b8521f36cb517f7c28/docs/whitedoc/gas_consumption.rst).

According to these approximations, the formula for the total gas consumption is (using `α` and `β` as scaling factors for the expense of reading, serialising, and deserialising storage and code):

```
α(size(code) + size(storage)) + cost(expensive_instructions) + (cost(average_instruction) x atomic_instructions) + βsize(storage)
```

In practice, as long as the contract code does not include costly loops with a large number of iterations, the cost of running the contract code is negligible compared to other costs.
In other words, **the gas consumption depends mostly on the total size of the contract code and storage** (and possibly a small number of expensive instructions, if any).
The amount of code _actually executed_ does not affect gas consumption as much.

### Expensive instructions

Compiling instructions to Michelson makes makes most simple, atomic instructions efficient with gas costs.
However, some instructions are disproportionately expensive.
Use these instructions wisely:

* `Tezos.get_contract_opt` and `Tezos.get_entrypoint_opt`: Converting an address to a typed contract costs a fixed 10000 gas units plus the cost for reading and deserialising the code of the called contract.
If the called contract is large, such an instruction may consume a lot more gas than you might expect.

* `Bytes.pack` and `Bytes.unpack`: These instructions involve serialising and deserialising values, so their cost depends on the size of the data.

* Reading and updating the values in a big-map may be more expensive than you expect because doing so involves serialising and deserialising values.
Also, big-map keys are stored as hashes, so reading or updating a value requires getting the hash of the key value.
However, unlike maps, the cost of reading or updating a big-map entry does not change as the big-map grows.

### Code size

The size of the contract code is often the most important optimisation target.
When you originate a large contract, you risk hitting an operation size limit and pay more for storing the code of the contract in the context.
The size of the contract matters in gas consumption as well: the bigger your contract is, the more gas is consumed for reading, deserialising, and type-checking it.

You can reduce your code size by:

- Simplifying the logic
- Separating larger or less frequently-used entrypoints into other contracts
- Changing the inlining of functions

You can use the `ligo info measure-contract` command to measure the size of the contract code:

```
ligo info measure-contract <SOURCE> --entry-point <ENTRYPOINT>
```

Also, to optimise code execution, you must ensure that the code doesn't have to run too many loop iterations.

### Storage size

The storage size and growth rate are also important optimisation targets.

Of course, the initial storage size affects how much the contract costs to deploy.
If the initial storage size is too large, the origination operation can exceed the gas limit.

The current size of the storage also affects the cost to call the contract because each time the contract is called, all non-lazy storage variables are read and deseralised, even if the transaction or called entrypoint does not use them.

The other major factor is how the storage grows over time.
As described above, the fee for each transaction includes a component to pay for the amount of storage increase from the maximum historical size of the contract storage.

:::note

As described in [Security](./security), it's possible for the storage to grow to a size that makes calling the contract impossible because the cost of reading the storage exceeds the gas limit.

:::

For these reasons, you must plan ahead to ensure that variables that can grow (such as lists, maps, and sets) cannot get too large.
In extreme cases, even variables like strings, numbers, and bytes can cause problems when they become very large.

In short, to reduce the cost of calling the contract many times over its lifespan:

- Reduce the amount of data that the contract stores.
- Make sure that the storage size stays the same or grows only when necessary.

### Using lazy storage

Using lazily-deserialized storage (big-maps) is a common way that smart contract developers reduce the cost of calling their contracts in the long run.
Unlike other variables, the big-maps in a contract's storage are not read and deserialized when the contract is called.
Instead, only the entries that the code accesses are read and deserialized.
Similarly, when you change a single entry in a big-map, the contract does not need to access the entire big-map, only that entry.

Therefore, while it costs more gas to read or update an entry from a big-map than from a regular map, the cost of reading or updating a big-map entry stays constant as the big-map grows.

Then why not just use big maps everywhere?
Accessing big map entries one-by-one is more expensive than just reading the whole storage in batch.
Moreover, big maps have limitations; for example, you can't iterate over the entries in a big-map, get a list of its keys, or even get a count of the number of entries in it.
To make your storage efficient, you must consider how big the values are, how often they are accessed, and what operations you need the storage to support.

Also, as described in [Security](./security), using non-lazy storage variables can expose your contract to attacks.
You should never allow users to directly increase the size of a non-lazy storage variable because they could grow that variable to a point where it prevents the contract from being called.
Therefore, if users can somehow increase the size of a storage variable, use a big-map or put a limit on the variable size.

In general, use lazy storage when any of these criteria are true:

- User interaction can grow the storage variable.

- The variable is large or unbounded.
The precise definition of "large" depends on what is in the variable and how many elements are accessed in transactions.

- You do not use the variable often.
For example, if a contract has many entrypoints but only one rarely-used entrypoint that needs the variable, you might want to use a big-map.
This way, calling the other entrypoints does not cause the variable to be loaded.

## Common optimisation techniques

### Constants optimisation

One of the most rewarding ways to optimise your contract is shrinking the constants. For example, if your contract has long, overly-verbose error descriptions passed to `Tezos.failwith`, you should consider replacing them with short abbreviated strings or even integer error codes.

If you have repeating constants (e.g., you may have several entrypoints that check permissions and a constant "PERMISSION_DENIED" error), you can extract these constants to a top-level binding. In this case, the LIGO compiler will generate the code of the form:

| Michelson instruction             | Description                                |
|-----------------------------------|--------------------------------------------|
| `PUSH string "PERMISSION_DENIED"` | Push the error string to stack             |
| ...                               | ...                                        |
| `DIG n`                           | Get the n-th stack entry and put it on top |

This is cheaper than pushing the same string to stack every time it is needed. This string will be pushed to stack _every time_ the contract is called, regardless of whether the current entrypoint actually uses it. This will not increase gas consumption significantly since, as we discussed, the cost of _interpreting_ the instruction is relatively low. However, you can go further and save large constants in _storage_ or even in a big map.

### Inlining

Consider the following contract:

<Syntax syntax="cameligo">

```cameligo
let sum (x, y : int * int) = x + y

let main (parameter, storage : int * int) : operation list * int =
  ([], sum (parameter, storage))
```

</Syntax>


There are two major ways to represent functions (like `sum`) in Michelson. The first way is to first push the function `f` to the stack, and then execute it with the argument `(parameter, storage)`:

| Michelson instruction | Stack after the instruction                    |
|-----------------------|------------------------------------------------|
|                       | `(parameter, storage)`                         |
| `LAMBDA`<br/>`  (pair int int) int`<br/>`  { UNPAIR; ADD }` | `{ UNPAIR; ADD }`; `(parameter, storage)` |
| `SWAP`                | `(parameter, storage)`; `{ UNPAIR; ADD }`      |
| `EXEC`                | `parameter + storage`                          |
| `NIL operation`       | `[]`, `parameter + storage`                    |
| `PAIR`                | `([], parameter + storage)`                    |


The second way is to replace the function call (`LAMBDA`, `SWAP`, `EXEC` sequence) with the function body, or _inline_ the function:

| Michelson instruction | Stack after the instruction                    |
|-----------------------|------------------------------------------------|
|                       | `(parameter, storage)`                         |
| `UNPAIR`              | `parameter`; `storage`                         |
| `ADD`                 | `parameter + storage`                          |
| `NIL operation`       | `[]`; `parameter + storage`                    |
| `PAIR`                | `([], parameter + storage)`                    |

You may notice that in this case, inlining reduced the size of the contract.

Other declarations can be inlined as well. In this contract, the compiler may generate the code that does `PUSH int 4` twice (in case there is an `[@inline]` annotation), or `PUSH int 4; DUP` (if there is no instruction to inline this binding):

<Syntax syntax="cameligo">

```cameligo
let n = 4

let main (_, _ : unit * int) : operation list * int = [], n * n
```

</Syntax>


LIGO will automatically inline declarations if two conditions are met:
1. The declaration is only used once
2. The declaration is pure, i.e., it does not depend on the execution context or cause failure.

If any of these conditions is not met, LIGO will **not** inline the declaration. You may use the `[@inline]` attribute to force inlining if the declaration is used more than once. You cannot force inlining if the declaration is not pure.

Unfortunately, there is no general rule on when to inline your declarations: sometimes inlining may increase the size of the contract, but in some cases – decrease it.

Intuitively, inlining functions is useful if:
1. You are inlining a function with a complex argument or return type – lambdas in Michelson require an explicit type annotation, and if you inline a function, you can omit it.
2. The function is not used often.

However, the best approach is to measure the gas consumption and the size of your contract to make a decision on inlining.

### Lazy-loading
This peculiar technique can be used to lower the average gas consumption of your contract by making large entrypoints a bit more expensive to call.

Imagine you have a contract with a number of small frequently-used entrypoints and several large entrypoints that are called rarely. During each transaction to the contract, the bakers would read **the whole code** of your contract, deserialise and type-check it, and only after that, execute the requested entrypoint.

<Syntax syntax="cameligo">

It turns out we can do better. Tezos has a lazy container – big map. The contents of big map are read, deserialised and type-checked during the call to `Big_map.find_opt`, and not at the beginning of the transaction. We can use this container to store the code of our heavy entrypoints: we need to add a `(bool, entrypoint_lambda) big_map` to the storage record, and then use `Big_map.find_opt` to fetch the code of the entrypoint from storage. (Note: in theory, we could use `(unit, entrypoint_lambda) big_map`, but, unfortunately, `unit` type is not comparable, so we cannot use it as a big map index).

Here is how it looks like:
```cameligo
type storage = { large_entrypoint : (bool, int -> int) big_map; result : int }

let load_large_ep (store : storage) : (int -> int) =
  let maybe_large_entrypoint =
    Big_map.find_opt true (store.large_entrypoint) in
  match maybe_large_entrypoint with
    Some ep -> ep
  | None -> failwith "Internal error"

[@entry]
let large_entry_point (n : int) (store :  storage) : operation list * storage =
  [], {store with result = (load_large_ep store) n}

(* Other entrypoints ... *)
```

</Syntax>

We can now put the code of this large entrypoint to storage upon the
contract origination. If we do not provide any means to change the
stored lambda, the immutability of the contract will not be affected.

<Syntax syntax="cameligo">

This pattern is also useful if you have long code blocks that repeat
across some subset of entrypoints. For example, if you develop a
custom token, you may need different flavors of transfers with a
common pre-transfer check. In this case, you can add a lambda
`preTransferCheck : (transfer_params -> bool)` to the storage and call
it upon transfer.

</Syntax>


However, you always need to measure the gas consumption and the
occupied storage. It may be the case that the wrapper code that
extracts the lambda from storage and calls it is costlier than the
piece of code you are trying to optimise.

## Conclusion

We have discussed the Tezos fee and gas model and identified the
following optimisation targets: contract and storage size, gas
consumption, and excess bytes written to storage. We also discussed
inlining, constants optimisation, lazy storage, and lazy entrypoint
loading. We hope these techniques can help you develop contracts that
require fewer resources to execute. And, we cannot stress this enough:
**always measure your contracts.**
