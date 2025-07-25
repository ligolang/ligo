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

:::warning

Contracts become unusable if the cost of calling them exceeds the gas limit.
For example, if the storage gets too large to load, deserialize, and type-check within the gas limit, the contract becomes permanently locked and unusable.

:::

## Measuring gas consumption

You can use the `ligo info measure-contract` command to get the size of a compiled contract's code in bytes:

```bash
ligo info measure-contract <SOURCE>
```

The `Test.Originate.contract` function also returns the size of the compiled contract in bytes.

There are two main ways to estimate the gas cost of calling a smart contract:

- In a test, the functions `Test.Contract.transfer` and `Test.Contract.transfer_exn` return the gas cost of successful smart contract calls.

- You can deploy the contract to a sandbox or test network and call it with the Octez client or add the `--dry-run` argument to simulate calling it, as in this example:

   ```bash
   octez-client call <CONTRACT_ADDRESS> from <ACCOUNT_OR_ALIAS> --entrypoint "<ENTRYPOINT>" --arg "<PARAMETER>" --dry-run --burn-cap 1
   ```

   The logging information includes the gas cost at the end, as in this example:

   ```
   Storage size: 144 bytes
   Paid storage size diff: 12 bytes
   Consumed gas: 1290.264
   Balance updates:
     tz1QCVQinE8iVj1H2fckqx6oiM85CNJSK9Sx ... -ꜩ0.003
     storage fees ........................... +ꜩ0.003
   ```

   In this log, the "consumed gas" is the total transaction fee (execution and storage) and the "storage fees" is the storage fee by itself.
   If the storage fee is not listed, the transaction did not increase storage and therefore did not incur a storage fee.

It's harder to get the cost of storage, but you can estimate it by deploying a contract with that storage to a sandbox or testnet.
Then you can send transactions to the contract to see the storage fees or deploy it different times with different initial storage values to get estimates of what the storage fees cost in the long run.

When you deploy a contract and provide an initial storage value, the log shows two storage fees.
The first fee is for the initial storage and the second is for the storage of the contract code itself.
You can use this information to optimize storage values and other variables such as parameters.

## Optimisation targets

For the reasons listed above, smart contract developers can focus on these optimisation targets:

1. Gas consumption
2. Contract code size
3. Total storage size
4. Amount of extra bytes written to storage

Another two factors – parameter size and operations pressure – are mostly out of control of the contract author.

Although the optimisation targets listed above are interrelated, you can look at them in isolation because the optimisation methods may differ.

### Gas consumption

Contrary to gas fees on some other blockchains where each instruction has a fixed cost, Tezos gas fees reflect actual computations and I/O operations performed by the nodes.
This gas cost accounting prevents vulnerabilities caused by incorrect estimation of the instruction costs, but it requires more calculation to get the actual gas cost of an operation.

To understand how gas is spent, consider the phases of transaction execution:

1. Reading the serialised contract code and non-lazy storage from the context
2. Deserialising the bytes into an intermediate representation
3. Converting untyped intermediate representation of code and storage into typed values
4. Running the contract code
5. Serialising the result of the computation
6. Writing the updated storage bytes back to the context

At each phase, a certain amount of gas is consumed.

* The amount of gas consumed in phases 1–3 depends on the size of the code, the total size of the non-lazy storage, and the size of the lazy storage that is accessed.

* The cost of running the contract code (phase 4) depends on the number of instructions and the complexity of those instructions.

  - You can estimate the cost of simple, atomic instructions like variable assignments and comparisons based on the average gas cost of a Michelson instruction because they don't differ very much.

  - You should estimate the cost of more complex, expensive instructions such as `Tezos.get_contract_opt` and `Bytes.pack` separately.

* The amount of gas consumed in phases 5–6 depends on the size of the non-lazy storage and the changes to the lazy storage.

These are only approximations; for example, the true cost of deserialization also depends on the inherent complexity of the code and data types, but the primary variable is the size.
Similarly, not all simple, atomic instructions cost the same amount.
For detailed info on gas consumption, please refer to the [Tezos gas model description](https://gitlab.com/tezos/tezos/-/blob/52a074ab3eb43ad0087804b8521f36cb517f7c28/docs/whitedoc/gas_consumption.rst).

According to these approximations, the formula for the total gas consumption is (using `α` and `β` as scaling factors for the expense of reading, serialising, and deserialising storage and code):

```
α(size(code) + size(storage)) + cost(expensive_instructions) + (cost(average_instruction) x atomic_instructions) + βsize(storage)
```

In practice, as long as the contract code does not include expensive instructions or costly loops with a large number of iterations, the cost of running the contract code is negligible compared to other costs.
In other words, **the gas consumption depends mostly on the total size of the contract code and storage**.
The amount of code _actually executed_ does not affect gas consumption as much.

:::note

Because of how gas consumption is calculated, optimising Tezos smart contracts can be very different from other kinds of code.
For example, when you optimise off-chain programs, you might spend more time on code that runs frequently and ignore code that runs infrequently.
However, when you optimise Tezos smart contracts, you must consider even code that runs infrequently because Tezos loads the entire code of the contract, not just the code that runs.
For this reason, you must pay attention to all entrypoints and functions, even if they are not called frequently.

:::

### Expensive instructions

Compiling instructions to Michelson makes makes most simple, atomic instructions efficient with gas costs.
However, some instructions are disproportionately expensive.
Use these instructions wisely:

* `Tezos.get_contract_opt` and `Tezos.get_entrypoint_opt`: Converting an address to a typed contract costs a fixed 10000 gas units plus the cost for reading and deserialising the code of the called contract.
If the called contract is large, such an instruction may consume a lot more gas than you might expect.

* `Bytes.pack` and `Bytes.unpack`: These instructions involve serialising and deserialising values, so their cost depends on the size of the data.

* Reading and updating the values in a big-map may be more expensive than you might expect because doing so involves fetching the value from lazy storage and serialising and deserialising values.
Also, big-map keys are stored as hashes, so reading or updating a value requires getting the hash of the key value.
However, unlike maps, the cost of reading or updating a big-map entry does not change as the big-map grows.

### Code size

The size of the contract code is often the most important optimisation target because each time a contract is called, it is read, deserialized, and type-checked.
For this reason, reducing the size of the contract code can yield large savings over its lifetime.
Also, originating a large contract costs more and (when combined with its initial storage) can exceed the gas limit.

You can reduce your code size by:

- Simplifying the logic
- Separating larger or less frequently-used entrypoints into other contracts
- Changing the inlining of functions

Also, to optimise code execution, you must ensure that the code doesn't have to run too many loop iterations.

To verify the size of the compiled contract, use the `ligo info measure-contract` command as described in [Measuring gas consumption](#measuring-gas-consumption).
Measuring contracts in this way is the best way to know if you have actually reduced the contract's size as compiled to Michelson.

### Storage size

The storage size and growth rate are also important optimisation targets.

Of course, the initial storage size affects how much the contract costs to deploy.
If the initial storage size is too large, the origination operation can exceed the gas limit.

The current size of the storage also affects the cost to call the contract because each time the contract is called, all non-lazy storage variables are read, deseralised, and type-checked, even if the transaction or called entrypoint does not use them.

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

You can also reduce the storage size by storing data in creative ways, such as by using lazy storage types (as described below) or by storing large or infrequently-used pieces of data in other contracts.

### Using lazy storage

Using lazily-deserialized storage (big-maps) is a common way that smart contract developers reduce the cost of calling their contracts in the long run.
Unlike other variables, the big-maps in a contract's storage are not read and deserialized when the contract is called.
Instead, only the entries that the code accesses are read and deserialized.
Similarly, when you change a single entry in a big-map, the contract does not need to access the entire big-map, only that entry.

Therefore, while it costs more gas to read or update an entry from a small big-map than from a comparable regular map, the cost of reading or updating a big-map entry stays constant as the big-map grows, unlike the regular map.

:::note

Then why not use big-maps everywhere?
Accessing big-map entries one-by-one is more expensive than reading the whole storage in batch, as in regular maps.
Moreover, big-maps have limitations; for example, you can't iterate over the entries in a big-map, get a list of its keys, or even get a count of the number of entries in it.
To choose between lazy and non-lazy storage, you must consider how big the values are, how often they are accessed, and what operations you need the storage to support.

:::

As described in [Security](./security), using non-lazy storage variables can expose your contract to attacks.
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

Many common techniques for optimising code also work for LIGO contracts.
The following sections show some specific ways that you can optimise LIGO contract code.

:::note

Always test the behaviour and gas consumption of optimisation efforts to ensure that the new code works and is more efficient than the original code that you are trying to optimise.

:::

### Reusing constants

You can often save a lot of code size with minimal effort by shrinking constants.
For example, if your contract has long error descriptions passed to `Tezos.failwith`, you can replace them with short abbreviated strings or integer error codes.

If the contract uses a constant in more than one place, you can reduce code size by making it a top-level variable.
For example, if your code has multiple entrypoints that check permissions and generate the same "PERMISSION_DENIED" errors, you might put a string variable with that message in each entrypoint.
When Tezos loads the contract code, it adds all of those variables to the Michelson code stack.
Instead, if you extract those messages to a single top-level variable, Tezos adds it to the stack only once and accesses it with a `DIG` instruction.

In this case, the LIGO compiler generates Michelson code that looks like this:

| Michelson instruction             | Description                                |
|-----------------------------------|--------------------------------------------|
| `PUSH string "PERMISSION_DENIED"` | Push the error string to stack             |
| ...                               | ...                                        |
| `DIG n`                           | Get the n-th stack entry and put it on top |

There might be multiple `DIG n` instructions on the stack for each time that the constant is used, but each is still more efficient than pushing the constant to the stack again.
You can go further and save large constants in storage or even in a big-map.

These optimisations may not increase gas consumption significantly because the cost of interpreting these instructions is already relatively low.
However, over the life of the contract, these small optimisations can add up.

### Inlining

Inlining is the process of embedding the code of a function instead of storing the function as a separate block of code.

Consider the following contract:

<Syntax syntax="cameligo">

```cameligo group=inlining_a
let sum (x, y : int * int) = x + y

let main (parameter, storage : int * int) : operation list * int =
  ([], sum (parameter, storage))
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=inlining_a
const sum = (x: int, y: int) => x + y;

const main = (parameter: int, storage: int): [list<operation>, int] =>
  [[], sum(parameter, storage)]
```

</Syntax>

There are two major ways to represent functions (like `sum` in the previous example) in Michelson. The first way is to push the function to the stack as a lambda and execute it with the argument `(parameter, storage)`:

| Michelson instruction | Stack after the instruction                    |
|-----------------------|------------------------------------------------|
|                       | `(parameter, storage)`                         |
| `LAMBDA`<br/>`  (pair int int) int`<br/>`  { UNPAIR; ADD }` | `{ UNPAIR; ADD }`; `(parameter, storage)` |
| `SWAP`                | `(parameter, storage)`; `{ UNPAIR; ADD }`      |
| `EXEC`                | `parameter + storage`                          |
| `NIL operation`       | `[]`, `parameter + storage`                    |
| `PAIR`                | `([], parameter + storage)`                    |

The second way is to put the individual instructions in the function on the stack, or _inline_ the function:

| Michelson instruction | Stack after the instruction                    |
|-----------------------|------------------------------------------------|
|                       | `(parameter, storage)`                         |
| `UNPAIR`              | `parameter`; `storage`                         |
| `ADD`                 | `parameter + storage`                          |
| `NIL operation`       | `[]`; `parameter + storage`                    |
| `PAIR`                | `([], parameter + storage)`                    |

In this case, inlining reduced the size of the contract.

Other declarations can be inlined as well.
In the following example, the compiler might generate the code that does `PUSH int 4` twice (in case there is an `[@inline]` annotation), or `PUSH int 4; DUP` (if there is no instruction to inline the code):

<Syntax syntax="cameligo">

```cameligo group=inlining_b
let n = 4

let main (_p, _s : unit * int) : operation list * int = [], n * n
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=inlining_b
const main = (_p: unit, _s: int): [list<operation>, int] => {
  const n = 4 as int;
  return [[], n * n];
};
```

</Syntax>

As described in [Inlining](../syntax/functions#inlining), LIGO automatically inlines declarations if both of these conditions are met:

- The declaration is used only once
- The declaration is pure, which means that it does not depend on the execution context or cause failure

If any of these conditions is not met, LIGO does not automatically inline the declaration.
You can use the `[@inline]` attribute or `@inline` decorator to force inlining if the declaration is used more than once.
You cannot force inlining if the declaration is not pure.

There is no general rule on when to inline your declarations.
Sometimes inlining increases the size of the contract and other times it decreases it.
The only way to be sure is to try both ways and use the `ligo info measure-contract` command and gas cost testing to compare.

In general, inlining functions is useful if:

- You are inlining a function with a complex argument or return type.
Lambdas in Michelson require an explicit type annotation, and if you inline a function, you can omit that type annotation.
- The function is not used often.

However, the best approach is to measure the gas consumption and the size of your contract to make a decision on inlining.

### Lazy-loading

Because the entire code of the contract is loaded each time it is called, you can sometimes reduce long-term gas costs by putting the code of large or infrequently-used entrypoints in other contracts.
Another way to achieve the same effect is by putting the code of these entrypoints or other logic in lambdas and store them in a big-map.

LIGO provides a system for storing logic in big-maps; see [Dynamic entrypoints](../syntax/contracts/dynamic-entrypoints).
If you don't want to to use dynamic entrypoints, you can do something similar manually by storing logic in big-maps.

For example, this contract has an entrypoint named `large_entry_point` that loads a large lambda from a big-map and runs it.
The contract could have other entrypoints that don't need the logic from that lambda.
Storing the logic in the big-map makes calling the `large_entry_point` entrypoint more expensive, but it makes calling the other entrypoints cheaper because the lambda isn't loaded.

<Syntax syntax="cameligo">

```cameligo group=lazy_entrypoints
module LazyEntrypoint = struct
  type storage_type = {
    large_entrypoint_map : (bool, int -> int) big_map;
    value : int
  }
  type return_type = operation list * storage_type

  (* Load the code from the big-map *)
  let load_large_ep (storage : storage_type) : (int -> int) =
    let large_entrypoint_opt =
      Big_map.find_opt true storage.large_entrypoint_map in
    match large_entrypoint_opt with
      Some ep -> ep
    | None -> failwith "Internal error"

  (* Run the code from the big-map *)
  [@entry]
  let large_entry_point (param : int) (storage : storage_type) : return_type =
    [], {storage with value = (load_large_ep storage) param}

  (* Do something that doesn't require the large code *)
  [@entry]
  let small_entry_point (param : int) (storage : storage_type) : return_type =
    [], {storage with value = param}

  (* Other entrypoints... *)

end
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=lazy_entrypoints
type big_lambda = (p: int) => int;
type storage_type = {
    large_entrypoint_map: big_map<bool, big_lambda>;
    value: int
  }
type return_type = [list<operation>, storage_type];

class LazyEntrypoint {

  // Get the code from the big-map
  static load_large_ep = (storage: storage_type): big_lambda => {
    const large_entrypoint_opt =
      Big_map.find_opt(true, storage.large_entrypoint_map);
    return $match(large_entrypoint_opt, {
      "Some": ep => ep,
      "None": () => failwith("Internal error"),
    });
  }

  // Run the code from the big-map
  @entry
  static large_entry_point = (param: int, storage: storage_type): return_type => {
    const newValue = load_large_ep(storage)(param);
    return [[], {
      large_entrypoint_map: storage.large_entrypoint_map,
      value: newValue,
      }];
  }

  // Do something that doesn't require the large code
  @entry
  static sub = (value: int, storage: storage_type): return_type =>
    [[], {
      large_entrypoint_map: storage.large_entrypoint_map,
      value: value,
      }];

  // Other entrypoints...

}
```

</Syntax>

You can now originate the contract and put the code of the lambda in the initial storage.
If the contract does not provide any means to change the stored lambda, the contract code remains immutable just like other smart contracts.

This pattern can also be useful if you have long code blocks that repeat across some subset of entrypoints.
For example, if you develop a custom token, you may need different flavors of transfers with a common pre-transfer check.
In this case, you can add a lambda for the pre-transfer check to a big-map and call it any time a transfer happens.
