---
id: contracts
title: Contracts
---

import Syntax from '@theme/Syntax';

We explain very briefly here what the blockchain Tezos is, what smart
contracts are, what addresses and operations are, etc.

### Tezos in a nutshell

The economic protocol of Tezos handles a ledger of transactions as a
*blockchain*. In a nutshell: a replicated database with strong access
control, immutable entries and resilience to malicious users.

Those transactions consist in the exchange of assets (tokens) between
peers of the network. Each peer has an address and an account (an
amount of tokens), but a physical person can be several
peers.

Public-key cryptography is used to secure the identity of the senders
of transactions and to ensure that past transactions are not tempered
with. An address is the hash of the peer's public key.

The business logic between the peers is *not* recorded in the chain,
that is, the knowledge of what they agree should trigger transfers of
tokens. The economic protocol of Tezos offers the possibility to
record in the chain that logic as a _smart contract_.

### Smart contracts in a nutshell

A smart contract is a peer that also has a program associated to a
private storage. That storage is writable only by the contract, but is
publicly readable. The smart contract is executed each time a specific
transaction is sent to its address. The transaction may include some
parameters.

Any call to a contract in a block is replicated by all the nodes in
the chain, to check whether the block is valid before including it in
their local view of the head of the chain. Once the block is
validated, it is broadcasted by the underlying peer-to-peer, gossip
network (as usual). The validation of smart contracts therefore adds a
*delay* in the validation of a block: the execution time of all the
smart contracts in it must fit the interblock time for the chain.

Therefore, each smart contract is allowed a given and fixed quantity
of computation, measured in *gas*.

Each instruction has an associated cost expected to be proportional to
the wall-clock time and an _ad hoc_ estimation is given by the node's
client.

A *node* is a server process that is accessed by a command-line client
or RPCs. A node comprises a view of the chain so far, the *context of
the chain* (a map from addresses to token amounts, used to validate
transactions), and the *mempool*.

When the execution exceeds the alloted gas, it is stopped, its effect
on the storage is rolled back, a failed transaction is included in the
block, and thus fees are collected to discourage spamming attacks.

The economic protocol sets limits on gas per block and per
transaction. Each smart contract has a *code size* and a *storage
size*, which are allocated on every node on the chain (in their
context). To limit the memory needed to synchronise the chain and
store it, a fee is set per byte and collected when a contract is
deployed (_originated_, in Tezos parlance).

It is only at the normal termination of a contract that the effects of
a contract becomes atomically visible to the network: the storage may
appear modified and a list of *operations* may have been returned
(transactions, contract creations and delegate settings) and
validated. In particular, a smart contract can transfer tokens to
other smart contracts, enabling the design of complex distributed
applications in Tezos. Note that, in Tezos, a transaction (transfer of
assets) is a special case of operation, and smart contracts can also
trigger the creation of other contracts and set delegates for
accounts.

Operations that are produced by a smart contract are said to be
_internal_, as opposed to the _external_ transactions that a smart
contract may receive.
