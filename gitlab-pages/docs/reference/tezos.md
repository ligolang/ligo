---
id: tezos-reference
title: tezos
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';


Tezos-specific functions

[module Operation](tezos.operation.md)

[module View](tezos.view.md)

[module Ticket](tezos.ticket.md)

[module Sapling](tezos.sapling.md)

<SyntaxTitle syntax="cameligo">
val call_view : &#39;param &#39;return.string -&gt; &#39;param -&gt; address -&gt; &#39;return option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
call_view: &lt;param, return&gt;(view: string, param: param, addr: address) =&gt; option&lt;return&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `Tezos.call_view v p a` calls the view `v` with parameter
`param` at the contract whose address is `a`. The value returned is
`None` if the view does not exist, or has a different type of
parameter, or if the contract does not exist at that
address. Otherwise, it is `Some v`, where `v` is the return value of
the view. Note: the storage of the view is the same as when the
execution of the contract calling the view started.

</Syntax>

<Syntax syntax="jsligo">

The call `Tezos.call_view(v, p, a)` calls the view `v` with
parameter `param` at the contract whose address is `a`. The
value returned is `["None" as "None"]` if the view does not
exist, or has a different type of parameter, or if the contract
does not exist at that address. Otherwise, it is `["Some" as "Some", v]`,
where `v` is the return value of the view. Note: the storage of
the view is the same as when the execution of the contract
calling the view started.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get_sender : unit -&gt; address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
get_sender: (_: unit) =&gt; address
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `Tezos.get_sender ()` is the address of the contract (that
is, a smart contract or an implicit account) that initiated the
current internal transaction. Note that, if transactions have been
chained, that address could be different from `Tezos.get_source ()`.

</Syntax>

<Syntax syntax="jsligo">

The call `Tezos.get_sender()` is the address of the contract (that is,
a smart contract or an implicit account) that initiated the current
internal transaction. Note that, if transactions have been chained,
that address could be different from `Tezos.get_source()`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get_source : unit -&gt; address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
get_source: (_: unit) =&gt; address
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `Tezos.get_source ()` is the address of the implicit account
that initiated the current transaction. If transactions have been
chained, that address is different from `Tezos.get_sender ()`.

</Syntax>

<Syntax syntax="jsligo">

The call `Tezos.get_source()` is the address of the implicit account
that initiated the current transaction. If transactions have been
chained, that address is different from `Tezos.get_sender()`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val self : &#39;a.string -&gt; &#39;a contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
self: &lt;a&gt;(entrypoint: string) =&gt; contract&lt;a&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `Tezos.self entrypoint` is the address of the current smart
contract, that is, the smart contract containing the call. For the
address of the smart contract actually *executing* the call, because
it is embedded in a lambda sent to another smart contract, use
`Tezos.get_self_address` instead. The string `entrypoint` is the name
of a valid entrypoint such that `entrypoint` is not `"%default"`, or
the empty string denoting the `"%default"` entrypoint (which is the
root of the smart contract parameter if no `"%default"` entrypoint is
explicitly defined). If the contract does not have the specified
entrypoint, the call results in an type checking error.

</Syntax>

<Syntax syntax="jsligo">

The call `Tezos.self(entrypoint)` is the address of the current smart
contract, that is, the smart contract containing the call. For the
address of the smart contract actually *executing* the call, because
it is embedded in a lambda sent to another smart contract, use
`Tezos.get_self_address` instead. The string `entrypoint` is the name
of a valid entrypoint such that `entrypoint` is not `"%default"`, or
the empty string denoting the `"%default"` entrypoint (which is the
root of the smart contract parameter if no `"%default"` entrypoint is
explicitly defined). If the contract does not have the specified
entrypoint, the call results in an type checking error.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get_self_address : unit -&gt; address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
get_self_address: (_: unit) =&gt; address
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `Tezos.get_self_address ()` is the address of the smart
contract actually executing the call, as a value of type
`address`. That contract can be different from the one containing the
call if the call is in a lambda transmitted to another smart
contract. Therefore, it is assumed that, in general, the type of the
executing contract is statically unknown, so the return type of
`Tezos.get_self_address` is not `'a contract`, but `address`. (See
`Tezos.self`.)

</Syntax>

<Syntax syntax="jsligo">

The call `Tezos.get_self_address()` is the address of the smart
contract actually executing the call, as a value of type
`address`. That contract can be different from the one containing the
call if the call is in a lambda transmitted to another smart
contract. Therefore, it is assumed that, in general, the type of the
executing contract is statically unknown, so the return type of
`Tezos.get_self_address` is not `'a contract`, but `address`. (See
`Tezos.self`.)

</Syntax>


<SyntaxTitle syntax="cameligo">
val address : &#39;a.&#39;a contract -&gt; address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
address: &lt;a&gt;(contract_addr: contract&lt;a&gt;) =&gt; address
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `Tezos.address contract` casts the address of the smart
contract `contract` into the more general value of type `address`.

</Syntax>

<Syntax syntax="jsligo">

The call `Tezos.address(contract)` casts the address of the smart
contract `contract` into the more general value of type `address`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val implicit_account : key_hash -&gt; unit contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
implicit_account: (_: key_hash) =&gt; contract&lt;unit&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `Tezos.implicit_account kh` casts the public key hash `kh`
into the address of its implicit account. Note that addresses of
implicit accounts always have the type `unit contract`.

</Syntax>

<Syntax syntax="jsligo">

The call `Tezos.implicit_account(kh)` casts the public key hash `kh`
into the address of its implicit account. Note that addresses of
implicit accounts always have the type `contract<unit>`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get_contract_opt : &#39;param.address -&gt; &#39;param contract option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
get_contract_opt: &lt;param&gt;(_: address) =&gt; option&lt;contract&lt;param&gt;&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `Tezos.get_contract_opt addr` casts the address `addr` into
that of a contract address, if such contract exists. The value of the
call is `None` if no such contract exists, otherwise `Some contract`,
where `contract` is the contract's address. Note: The address of an
implicit account has type `unit contract`.

</Syntax>

<Syntax syntax="jsligo">

The call `get_contract_opt(addr)` casts the address `addr` into
that of a contract address, if such contract exists. The value
of the call is `["None" as "None"]` if no such contract exists,
otherwise `["Some" as "Some", contract]`, where `contract` is
the contract's address. Note: The address of an implicit account
has type `contract<unit>`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get_contract_with_error : &#39;param.address -&gt; string -&gt; &#39;param contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
get_contract_with_error: &lt;param&gt;(_: address) =&gt; (_: string) =&gt; contract&lt;param&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `Tezos.get_contract_with_error addr error` casts the address
`addr` into that of a contract address, if such contract exists. If
not, the execution fails with the error message `error`.

</Syntax>

<Syntax syntax="jsligo">

The call `Tezos.get_contract_with_error(addr, error)` casts the
address `addr` into that of a contract address, if such contract
exists. If not, the execution fails with the error message `error`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get_contract : &#39;param.address -&gt; &#39;param contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
get_contract: &lt;param&gt;(_: address) =&gt; contract&lt;param&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `Tezos.get_contract addr` casts the address `addr` into that
of a smart contract address, if such contract exists. The call fails
with the message `"bad address for get_contract"` if no such smart
contract exists. Note: The address of an implicit account has type
`unit contract`.

</Syntax>

<Syntax syntax="jsligo">

The call `Tezos.get_contract(addr)` casts the address `addr` into that
of a smart contract address, if such contract exists. The call fails
with the message `"bad address for get_contract"` if no such smart
contract exists. Note: The address of an implicit account has type
`contract<unit>`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get_entrypoint_opt : &#39;param.string -&gt; address -&gt; &#39;param contract option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
get_entrypoint_opt: &lt;param&gt;(entrypoint: string, addr: address) =&gt; option&lt;contract&lt;param&gt;&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `Tezos.get_entrypoint_opt entrypoint addr` has the same
behaviour as `Tezos.get_contract_opt addr`, with the additional
constraint that the contract must have an entrypoint named
`entrypoint`. In other words, `Tezos.get_entrypoint_opt entrypoint
addr` casts the address `addr` into that of a smart contract address,
if such contract exists and has an entrypoint named `entrypoint`. The
value of the call is `None` if no such smart contract exists,
otherwise `Some contract`, where `contract` is the smart contract's
address. Note: The address of an implicit account has type `unit
contract`.

</Syntax>

<Syntax syntax="jsligo">

The call `get_entrypoint_opt(entrypoint, addr)` has the same
behaviour as `get_contract_opt(addr)`, with the additional
constraint that the contract must have an entrypoint named
`entrypoint`. In other words, `get_entrypoint_opt(entrypoint,
addr)` casts the address `addr` into that of a smart contract
address, if such contract exists and has an entrypoint named
`entrypoint`. The value of the call is `["None" as "None"]` if
no such smart contract exists, otherwise `["Some" as "Some",
contract]`, where `contract` is the smart contract's
address. Note: The address of an implicit account has type
`contract<unit>`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get_entrypoint : &#39;param.string -&gt; address -&gt; &#39;param contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
get_entrypoint: &lt;param&gt;(entrypoint: string, addr: address) =&gt; contract&lt;param&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `get_entrypoint entrypoint addr` casts the address `addr`
into that of a smart contract address, if such contract exists and has
an entrypoint named `entrypoint`. If no such smart contract exists,
the execution fails with the error message `"bad address for
get_entrypoint"`. Note: The address of an implicit account has type
`unit contract`.

</Syntax>

<Syntax syntax="jsligo">

The call `get_entrypoint(entrypoint, addr)` casts the address `addr`
into that of a smart contract address, if such contract exists and has
an entrypoint named `entrypoint`. If no such smart contract exists,
the execution fails with the error message `"bad address for
get_entrypoint"`. Note: The address of an implicit account has type
`contract<unit>`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val open_chest : chest_key -&gt; chest -&gt; nat -&gt; bytes option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
open_chest: (key: chest_key, _: chest, time: nat) =&gt; option&lt;bytes&gt;
</SyntaxTitle>

The function `open_chest` opens a timelocked chest given its key and
the time. The result is a byte option depending if the opening is
correct or not.


<SyntaxTitle syntax="cameligo">
val get_balance : unit -&gt; tez
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
get_balance: (_: unit) =&gt; tez
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `Tezos.get_balance ()` returns the balance in mutez of the
account associated to the currently executed smart contract, including
any mutez added by the calling transaction.

</Syntax>

<Syntax syntax="jsligo">

The call `Tezos.get_balance()` returns the balance in mutez of the
account associated to the currently executed smart contract, including
any mutez added by the calling transaction.

</Syntax>

<SyntaxTitle syntax="cameligo">
val get_amount : unit -&gt; tez
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
get_amount: (_: unit) =&gt; tez
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `Tezos.get_amount ()` returns the amount in mutez of the
current transaction.

</Syntax>

<Syntax syntax="jsligo">

The call `Tezos.get_amount()` returns the amount in mutez of the
current transaction.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get_now : unit -&gt; timestamp
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
get_now: (_: unit) =&gt; timestamp
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `Tezos.get_now ()` returns the minimal injection time for the
current block, namely the block whose application triggered this
execution. The minimal injection time constitutes an estimate of the
moment when the current block is injected, hence the name "now".

</Syntax>

<Syntax syntax="jsligo">

The call `Tezos.get_now()` returns the minimal injection time for the
current block, namely the block whose application triggered this
execution. The minimal injection time constitutes an estimate of the
moment when the current block is injected, hence the name "now".

</Syntax>


<SyntaxTitle syntax="cameligo">
val get_min_block_time : unit -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
get_min_block_time: (_: unit) =&gt; nat
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `Tezos.get_min_block_time ()` returns the minimal delay
between two consecutive blocks in the chain.

</Syntax>

<Syntax syntax="jsligo">

The call `Tezos.get_min_block_time()` returns the minimal delay
between two consecutive blocks in the chain.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get_level : unit -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
get_level: (_: unit) =&gt; nat
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `Tezos.get_level ()` returns the current block level.

</Syntax>

<Syntax syntax="jsligo">

The call `Tezos.get_level()` returns the current block level.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get_chain_id : unit -&gt; chain_id
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
get_chain_id: (_: unit) =&gt; chain_id
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `Tezos.get_chain_id ()` returns the identifier of the chain
on which the smart contract is executed.

</Syntax>

<Syntax syntax="jsligo">

The call `Tezos.get_chain_id ()` returns the identifier of the chain
on which the smart contract is executed.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get_total_voting_power : unit -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
get_total_voting_power: (_: unit) =&gt; nat
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `Tezos.get_total_voting_power ()` returns the total voting
power of all contracts. The total voting power coincides with the sum
of the stake of every contract in the voting listings. The voting
listings is calculated at the beginning of every voting period.

</Syntax>

<Syntax syntax="jsligo">

The call `Tezos.get_total_voting_power()` returns the total voting
power of all contracts. The total voting power coincides with the sum
of the stake of every contract in the voting listings. The voting
listings is calculated at the beginning of every voting period.

</Syntax>


<SyntaxTitle syntax="cameligo">
val voting_power : key_hash -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
voting_power: (_: key_hash) =&gt; nat
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `Tezos.voting_power contract_kh` returns the voting power of
a given contract specified by the key hash `contract_kh`. This voting
power coincides with the weight of the contract in the voting listings
(that is, the stake) which is calculated at the beginning of every
voting period.

</Syntax>

<Syntax syntax="jsligo">

The call `Tezos.voting_power(contract_kh)` returns the voting power of
a given contract specified by the key hash `contract_kh`. This voting
power coincides with the weight of the contract in the voting listings
(that is, the stake) which is calculated at the beginning of every
voting period.

</Syntax>


<SyntaxTitle syntax="cameligo">
val never : &#39;a.never -&gt; &#39;a
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
never: &lt;a&gt;(_: never) =&gt; a
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `Tezos.never n` is never meant to be executed, as the type
`never` is inhabited, but to instruct the typechecker that a branch in
the control flow, for example, in a pattern matching, is dead.

</Syntax>

<Syntax syntax="jsligo">

The call `Tezos.never(n)` is never meant to be executed, as the type
`never` is inhabited, but to instruct the typechecker that a branch in
the control flow, for example, in a pattern matching, is dead.

</Syntax>


<SyntaxTitle syntax="cameligo">
val pairing_check : (bls12_381_g1 * bls12_381_g2) list -&gt; bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
pairing_check: (_: list&lt;[bls12_381_g1, bls12_381_g2]&gt;) =&gt; bool
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `Tezos.pairing_check pairings` verifies that the product of
pairings of the given list of points `pairings` is equal to 1 in the
field Fq12. It evaluates in `true` if the list is empty. This function
can be used to verify if two pairings P1 and P2 are equal by verifying
P1 * P2^(-1) = 1.

</Syntax>

<Syntax syntax="jsligo">

The call `Tezos.pairing_check(pairings)` verifies that the product of
pairings of the given list of points `pairings` is equal to 1 in the
field Fq12. It evaluates in `true` if the list is empty. This function
can be used to verify if two pairings P1 and P2 are equal by verifying
P1 * P2^(-1) = 1.

</Syntax>


<SyntaxTitle syntax="cameligo">
val constant : &#39;a.string -&gt; &#39;a
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
constant: &lt;a&gt;(hash: string) =&gt; a
</SyntaxTitle>

<Syntax syntax="cameligo">

The call to `Tezos.constant hash` returns the value stored on-chain
whose hash value is `hash` (global constants). This call can fail when
the contract is originated if the hash is invalid or the expansion of
the global constant is ill-typed, or too large (gas consumption).

</Syntax>

<Syntax syntax="jsligo">

The call to `Tezos.constant(hash)` returns the value stored on-chain
whose hash value is `hash` (global constants). This call can fail when
the contract is originated if the hash is invalid or the expansion of
the global constant is ill-typed, or too large (gas consumption).

</Syntax>
