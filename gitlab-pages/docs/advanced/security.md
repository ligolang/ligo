---
id: security
title: Smart contract security
---

import Syntax from '@theme/Syntax';

Web3 developers need to keep some specific vulnerabilities in mind when they write on-chain and off-chain applications.
This page covers the basics of smart contract security on Tezos, some of these potential vulnerabilities, and how to protect your contracts against them.

:::note

This guide is aimed at giving the reader an overview of popular attacks on smart contracts and distributed applications.
It is not an exhaustive list of all the possible attack vectors.
Use your own judgement, good programming practice, and thorough testing on your contracts.

The descriptions in this document are valid for the Tezos protocol since the Edo upgrade, last updated at the Rio upgrade.
Because Tezos is an upgradeable blockchain, some of the blockchain mechanics may change when new protocols are adopted.
For this reason, Tezos developers must stay up to date on the changes in the protocol via sources such as the Octez and protocol documentation at https://octez.tezos.com.

:::

See these links for more information about security on Tezos applications:

- The smart contracts section on https://opentezos.com
- The tutorial [Learn and play with security](https://docs.tezos.com/tutorials/security) on docs.tezos.com

## Resource constraints

Tezos limits the size of an operation so that nodes can broadcast operations over the network in a reasonable time.
It also places a limit on the computations that bakers need to perform to validate an operation to keep the network running smoothly.
This limit is called the *gas limit* because it is the maximum amount of computations (measured in *gas units*) that a single operation can require.

Of course, developers make their contracts efficient to save on gas fees, but they must also keep the gas limit in mind because it can lead to security vulnerabilities.

For example, look at this seemingly innocent wallet contract that stores an event log:

<Syntax syntax="cameligo">

```cameligo group=walletwithflaw
module WalletWithFlaw = struct

  (* Variant for two types of transactions *)
  type transaction =
    Deposit of address * tez
  | Withdrawal of address * tez

  type storage = {
    owner : address;
    transactionLog : transaction list
  }

  type return_type = operation list * storage

  (* Receive a deposit *)
  [@entry]
  let deposit (_ : unit) (storage : storage) : return_type =
    (* Verify that tez was sent *)
    let _ = if Tezos.get_amount () = 0tez then failwith "Send tez to deposit" in
    (* Add log entry *)
    let newLogEntry : transaction = Deposit (Tezos.get_sender (), Tezos.get_amount ()) in
    [], { storage with transactionLog = newLogEntry :: storage.transactionLog }

  (* Return a withdrawal *)
  [@entry]
  let withdraw (tx_destination, tx_amount : address * tez) (storage : storage) : return_type =
    (* Verify that the sender is the admin *)
    let _ = if Tezos.get_sender () <> storage.owner then failwith "Not the owner" in
    (* Verify that no tez was sent *)
    let _ = if Tezos.get_amount () <> 0tez then failwith "Don't send tez to this entrypoint" in
    (* Create transaction *)
    let callee = Tezos.get_contract_opt tx_destination in
    let operation = match callee with
      Some contract ->
        Tezos.Operation.transaction () tx_amount contract
    | None -> failwith "Couldn't send withdrawal to that address"
    in
    (* Add log entry and return operation and new log *)
    let newLogEntry : transaction = Withdrawal (tx_destination, tx_amount) in
    [operation], { storage with transactionLog = newLogEntry :: storage.transactionLog }

end
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=walletwithflaw
namespace WalletWithFlaw {

  // Variant for two types of transactions
  type transaction =
    ["Deposit", [address, tez]]
  | ["Withdrawal", [address, tez]];

  type storage = {
    owner: address,
    transactionLog: list<transaction>,
  };

  type return_type = [list<operation>, storage];

  // Receive a deposit
  // @entry
  const deposit = (_: unit, storage: storage): return_type => {
    // Verify that tez was sent
    if (Tezos.get_amount() == (0 as tez)) {
      failwith("Send tez to deposit");
    }
    // Add log entry
    const newLogEntry: transaction = ["Deposit" as "Deposit", [Tezos.get_sender(), Tezos.get_amount()]];
    return [[], {
      owner: storage.owner,
      transactionLog: [newLogEntry, ...storage.transactionLog],
    }];
  }

  // Return a withdrawal
  // @entry
  const withdraw = (param : [address, tez], storage : storage): return_type => {
    const [tx_destination, tx_amount] = param;
    // Verify that the sender is the admin
    if (Tezos.get_sender() != storage.owner) {
      failwith("Not the owner");
    }
    // Verify that no tez was sent
    if (Tezos.get_amount() != (0 as tez)) {
      failwith("Don't send tez to this entrypoint");
    }
    // Create transaction
    const callee = Tezos.get_contract_opt(tx_destination);
    const operation = $match(callee, {
      "Some": contract => (() => Tezos.Operation.transaction(unit, tx_amount, contract))(),
      "None": () => failwith("Couldn't send withdrawal to that address"),
    });
    // Add log entry and return operation and new log
    const newLogEntry: transaction = ["Withdrawal" as "Withdrawal", [tx_destination, tx_amount]];
    return [[operation], {
      owner: storage.owner,
      transactionLog: [newLogEntry, ...storage.transactionLog],
    }];
  }
}
```

</Syntax>

This contract:

- Can receive funds sent to it via the `Deposit` entrypoint.
- Can send some tez to any account via the `Withdrawal` entrypoint callable by the owner.
- Stores a log of all transactions.

What can go wrong?
To see the flaw, you need to understand how Tezos processes transactions and what limits it places on them.

As described above, Tezos puts a limit on the amount of processing that a single transaction can require.
This processing includes loading all non-lazy variables in the contract's storage.
Each variable gets fetched, deserialised, and type-checked each time the contract is called, which requires computation.

Each time you call this contract, it adds a log entry to the `list` variable in the storage and therefore the storage is larger the next time that you call it.
This design flaw causes two problems:

- Calling this contract gets more expensive each time you call it
- Eventually the amount of processing required will exceed the maximum for a single transaction and thus it will be impossible to call the contract, making it unusable and locking the tez in it

Can you think of a way to fix this flaw while retaining the transaction log?
There are several ways, including:

- Storing the log off the chain or relying on an indexer to get a list of past transactions
- Using a lazy storage type such as a big-map, which is not loaded entirely when the contract is called
- Truncating the log to show only a few recent transactions or otherwise limiting the size of the log

In this way, you must plan ahead to limit the storage size of contracts as they grow.
Here are some other ways that storage size can cause problems:

- Unbounded types such as nats, integers, and bytes can become arbitrarily large.
These types are less likely to cause problems than lists and maps but still can.

- Lambdas in storage can grow or cause data storage issues, so you should never store untrusted lambdas.

Also, storage size isn't the only way that contracts can exceed the maximum gas and become unusable.
Lambdas or loops in your code can cause vulnerabilities by forcing future transactions to run a large loop or make too many computations, exceeding the gas limit.
You must consider both the storage and the logic of the contract to ensure that it will not exceed the gas limit in the long term.

## Transaction ordering

Blockchains use block producers (called *bakers* in Tezos) to put transactions into blocks.
Block producers are free to include or exclude transactions within the blocks they produce and to put transactions in any order.
Transactions run in the order that they are listed in the block, so in certain cases, block producers can manipulate the order of transactions to make a profit or cause a certain effect.

Also, bakers usually put transactions with higher transaction fees or lower counter values before transactions with lower fees.
Therefore, other actors can sometimes influence transaction ordering for their benefit.

Manipulating the transaction order like this happens very rarely, but it can cause problems for decentralised finance (DeFi) applications.

A classic example of a system vulnerable to this kind of attacks is a decentralised exchange with an on-chain orderbook.
This exchange accepts orders to buy and sell assets at a certain price and runs them in the order that it receives them, which depends on the order that the transactions are listed in each block.

In an attack known as *front-running*, an attacker may see a large transaction coming and use the methods described above to insert their transaction before that large transaction.
For example, they could see a large buy order coming and submit their own buy order with a high transaction fee to get it to run before the large one raises the price of the asset.
In fact, if the front-runner is a baker, the so-called _miner extracted value_ [poses a big risk](https://arxiv.org/pdf/1904.05234.pdf) to security of blockchains in general.

To defend against this kind of attack, you must prevent block producers and other users from profiting by manipulating the order of transactions.
For example, you could store the order book off-chain or use [timelocks](https://docs.tezos.com/smart-contracts/timelocks) to prevent attackers from seeing incoming transactions.

## Timestamps

Aside from transaction ordering, block producers can manipulate other variables that contracts rely on.
For example, block producers set the timestamp of each block that they create based on their own clocks.
In older versions of the Tezos protocol, the value of the `Tezos.get_now` value was this timestamp.
If a contract used the value of `Tezos.get_now` to get the time, block producers could manipulate the timestamp to change the behaviour of the contract.

In the current Tezos protocol, the value of `Tezos.get_now` is always the timestamp of the previous block plus a fixed value, regardless of the time that the block was actually created, which eliminates straightforward manipulations.
However, contracts that rely on timestamps are still vulnerable to manipulation.
In particular, contracts should never use timestamps or `Tezos.get_now` as a source of randomness.

## Reentrancy and call injection

As described in [Operations](../syntax/contracts/operation) and in [Operations](https://docs.tezos.com/smart-contracts/logic/operations) on docs.tezos.com, Tezos orders operations in a way that may appear unusual to developers who are unfamiliar with blockchains.
In particular, when an operation (such as a call to a smart contract) generates other operations, those operations do not run until the original operation completes.
In general, calls to smart contracts run in this order:

1. The smart contract runs its logic.
1. The smart contract returns any operations and events that it created and the new state of its storage.
1. The protocol updates the contract's storage based on the storage that it returned.
1. The protocol queues the operations and events that the smart contract returned and runs them in order.

Note that, based on this order, a smart contract cannot run operations or emit events in the middle of its own execution.
It can only queue operations and events to run after it finishes running.

See [Operations](https://docs.tezos.com/smart-contracts/logic/operations) on docs.tezos.com for examples of operation ordering.

This process is similar to the checks-effects-interactions pattern popular in Solidity.
In Ethereum, this process is considered a best practice, and Tezos enforces this on the protocol level with operation ordering.
Such restrictions help prevent *reentrancy attacks*, which take advantage of flaws in contracts by calling them at intermediate points in their execution.

Consider the following snippet in Solidity from a bank smart contract:

```solidity
function withdraw(uint256 amount) {
  uint256 balance = balances[beneficiary];
  require(balance >= amount);
  beneficiary.call.value(amount)();
  uint256 new_balance = balance - amount;
  balances[beneficiary] = new_balance;
}
```

This code follows these general steps:

1. A caller requests to withdraw funds from their account.
1. The smart contract checks that the requested amount is less than or equal to their bank balance.
1. The smart contract sends the withdrawn funds to the caller.
1. The smart contract updates the caller's balance in storage.

Note that the _effect_ of updating the storage happens after the _interaction_ (transferring the `amount` to the beneficiary).
As a result, this contract has a reentrancy vulnerability: If the contract execution pauses after the transfer starts but before the contract updates the caller's balance in storage, the caller could start another withdrawal and receive more funds before the first balance update happens.

The way that Tezos orders operations makes it difficult to run reentrancy attacks.
For example, here is an equivalent contract in LIGO:

<Syntax syntax="cameligo">

```cameligo group=bank
type storage = (address, tez) big_map
type return_type = operation list * storage

[@entry]
let withdraw (tx_amount : tez) (storage : storage) : return_type =
  (* Verify that the caller has enough balance for the withdrawal *)
  let old_balance = Big_map.find (Tezos.get_sender ()) storage in
  let _ = if tx_amount > old_balance then failwith "Insufficient balance" in
  (* Create transaction *)
  let receiver_account = match Tezos.get_contract_opt (Tezos.get_sender ()) with
    Some account -> account
  | None -> failwith "Couldn't find account" in
  let operation = Tezos.Operation.transaction unit tx_amount receiver_account in
  (* Update balance *)
  let new_balance : tez = Option.value_with_error "Unreachable error; we already compared balance to amount" (old_balance - tx_amount) in
  let new_storage = Big_map.update (Tezos.get_sender ()) (Some new_balance) storage in
  [operation], new_storage
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=bank
namespace Bank {

  type storage = big_map<address, tez>;
  type return_type = [list<operation>, storage];

  // Return a withdrawal
  // @entry
  const withdraw = (tx_amount: tez, storage: storage): return_type => {
    // Verify that the caller has enough balance for the withdrawal
    const old_balance = Big_map.find(Tezos.get_sender(), storage);
    if (tx_amount > old_balance) {
      failwith("Insufficient balance");
    }
    // Create transaction
    const receiver_account = $match((Tezos.get_contract_opt(Tezos.get_sender())), {
      "Some": account => account,
      "None": () => failwith("Couldn't find account"),
    });
    const operation = Tezos.Operation.transaction(unit, tx_amount, receiver_account);
    // Update balance
    const new_balance: tez = Option.value_with_error("Unreachable error; we already compared balance to amount", old_balance - tx_amount);
    const new_storage = Big_map.update(Tezos.get_sender(), ["Some" as "Some", new_balance], storage);
    return [[operation], new_storage];
  }

}
```

</Syntax>

The general steps of the code are similar, but the critical difference is that the operation to transfer funds (the `op` operation variable) is created but does not run immediately.
The smart contract returns it in the list of operations at the end of its execution and Tezos queues it to run later.
Tezos updates the balances in the contract storage before it runs any subsequent operations, preventing reentrancy attacks.

However, in some cases reentrancy attacks are still possible on Tezos, especially if contracts wait for a callback in an intermediate state.
For example, if the bank contract stores balances in a separate contract and updates them by sending transactions to that contract, it may be susceptible to reentrancy attacks.
Users may be able to manipulate the order of those transactions to run more than one withdrawal before the transactions update their balance.

## Transactions to untrusted contracts

When emitting a transaction to an untrusted contract, you can not assume that it will "play by the rules."
Instead, you should always bear in mind that the callee may fail, causing the entire operation to fail or emit other operations that you do not expect.

Consider this example, which keeps a list of addresses.
When the owner account calls the `send_rewards` entrypoint, it attempts to send 5 tez to each address:

<Syntax syntax="cameligo">

```cameligo group=rewardswithflaw
module RewardsWithFlaw = struct

  type storage = {
    owner : address;
    beneficiaries : address list
  }

  (* Send rewards to one address *)
  let send_one_reward (beneficiary_addr : address) : operation =
    let contract_opt =
      Tezos.get_contract_opt beneficiary_addr in
    let beneficiary =
      match contract_opt with
        Some contract -> contract
      | None -> (failwith "CONTRACT_NOT_FOUND" : unit contract) in
    Tezos.Operation.transaction () 5tez beneficiary

  (* Send rewards to all beneficiaries *)
  [@entry]
  let send_rewards (_ : unit) (storage : storage) : operation list * storage =
    if Tezos.get_sender () <> storage.owner
    then failwith "Not the owner"
    else let operations = List.map send_one_reward storage.beneficiaries in
    operations, storage

  [@entry]
  let change_owner (new_owner : address) (storage : storage) : operation list * storage =
    (* Verify that the sender is the admin *)
    let _ = if Tezos.get_sender () <> storage.owner then failwith "Not the owner" in
    [], { storage with owner = new_owner }

end
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=rewardswithflaw
namespace RewardsWithFlaw {

  export type storage = {
    owner: address,
    beneficiaries: list<address>,
  };

  // Send rewards to one address
  const send_one_reward = (beneficiary_addr: address): operation => {
    const contract_opt =
      Tezos.get_contract_opt( beneficiary_addr);
    const beneficiary = $match(contract_opt, {
      "Some": contract => contract,
      "None": () => failwith("CONTRACT_NOT_FOUND"),
    });
    return Tezos.Operation.transaction(unit, 5 as tez, beneficiary);
  }

  // Send rewards to all beneficiaries
  // @entry
  const send_rewards = (_: unit, storage: storage): [list<operation>, storage] => {
    if (Tezos.get_sender() != storage.owner) {
      failwith("Not the owner");
    }
    const operations = List.map(send_one_reward, storage.beneficiaries);
    return [operations, storage];
  }

  // @entry
  const change_owner = (new_owner: address, storage: storage): [list<operation>, storage] => {
    // Verify that the sender is the admin
    if (Tezos.get_sender() != storage.owner) {
      failwith("Not the owner");
    }
    return [[], {
      owner: new_owner,
      beneficiaries: storage.beneficiaries,
     }];
  }

}
```

</Syntax>

When the owner calls the `send_rewards` entrypoint, the contract attempts to create a list of operations.
If one of these attempts to create an operation fails because the receiving contract fails or does not exist, the entire call to the `send_rewards` entrypoint fails and no transfers happen.
Regardless of whether this failure is because of a mistake or intentional censorship, the contract is stuck.

In cases like these, allow users to withdraw funds independently instead of running operations in a batch.
This way, if one transfer fails, it does not affect other transfers.

## Incorrect authorisation checks

When developing a contract, you may want to restrict who can call a certain entrypoint.
In this case you must ensure that:

- The request comes from an authorised entity
- The authorised entity cannot be tricked into sending the request

To determine which account sends a request, you may be tempted to use the `Tezos.get_source` function.
This function returns the address of the account that submitted the operation that started a chain of operations, but not necessarily the account that sent the immediate transaction that the contract is running now.
Relying on `Tezos.get_source` can allow a malicious contract to impersonate an account when it calls another contract.

For example, assume that account A calls smart contract B, which generates an operation to call smart contract C.
When C runs, in Tezos terms, account A is the *source* of the transaction and smart contract B is the *sender* of the transaction.
Therefore, if it uses `Tezos.get_source` to check which account calls it, contract B could trick it into thinking account A called it.
In this way, a malicious contract can invite accounts to make seemingly innocent transactions and use the operation chain to impersonate those users in other transactions.

:::warning

For this reason, contracts should never use `Tezos.get_source` for authorisation purposes.

:::

For more information about senders and sources, see [Sender vs Source confusion](https://docs.tezos.com/tutorials/security/part-1#sender-vs-source-confusion) on docs.tezos.com.

Checking whether `Tezos.get_sender` (the address of the immediate caller) is authorised to perform an operation is better.
Because the request comes directly from an authorised entity, contracts can be more confident that the call is legitimate.
This approach is a good default choice if both conditions hold true:

1. The sender contract is well secured against emitting arbitrary operations.
For instance, it must not contain ["view" entrypoints](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-4/tzip-4.md#view-entrypoints) as defined in [TZIP-4](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-4/tzip-4.md).

2. You only need to authorise an immediate caller and not the contracts somewhere up in the call chain.

If either of these conditions is not met, you may need to use [tickets](../data-types/tickets) to authenticate requests.
Tickets can be transferred, but they always have the address of the contract that created them as their ticketer.

In this way, tickets allow you to verify that requests came from a certain contract.
For example, you can set up a contract that authenticates requests by requiring a ticket with the request, using the address of the ticketer to determine the permissions for the action, and using the data in the ticket as the parameters or input for the request.

In general, sender-based authorisation is appropriate only for simple scenarios, such as when the contract has a single "owner" address controlled by a user account.
In more complex scenarios, ticket-based authorisation is often better.
