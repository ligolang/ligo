---
id: tezos-reference
title: Tezos
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';


Tezos-specific functions


<SyntaxTitle syntax="cameligo">
val get&#95;sender : unit -&gt; address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;sender: (&#95;: unit) =&gt; address
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `get_sender ()` is the address of the contract (that
    is, a smart contract or an implicit account) that initiated the
    current internal transaction. Note that, if transactions have been
    chained, that address could be different from `get_source ()`.

</Syntax>

<Syntax syntax="jsligo">

The call `get_sender()` is the address of the contract (that
    is, a smart contract or an implicit account) that initiated the
    current internal transaction. Note that, if transactions have been
    chained, that address could be different from `get_source()`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get&#95;source : unit -&gt; address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;source: (&#95;: unit) =&gt; address
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `get_source ()` is the address of the implicit account
    that initiated the current transaction. If transactions have been
    chained, that address is different from `get_sender ()`.

</Syntax>

<Syntax syntax="jsligo">

The call `get_source()` is the address of the implicit account
    that initiated the current transaction. If transactions have been
    chained, that address is different from `get_sender()`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val self : &#39;a.string -&gt; &#39;a contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let self: &lt;a&gt;(&#95;: string) =&gt; contract&lt;a&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `self entrypoint` is the address of the current smart
    contract, that is, the smart contract containing the call. For the
    address of the smart contract actually *executing* the call,
    because it is embedded in a lambda sent to another smart contract,
    use `get_self_address` instead. The string `entrypoint` is the
    name of a valid entrypoint such that `entrypoint` is not
    `"default"`, or the empty string denoting the `"default"`
    entrypoint (which is the root of the smart contract parameter if
    no `"default"` entrypoint is explicitly defined). If the contract
    does not have the specified entrypoint, the call results in an
    type checking error.

</Syntax>

<Syntax syntax="jsligo">

The call `self(entrypoint)` is the address of the current smart
    contract, that is, the smart contract containing the call. For the
    address of the smart contract actually *executing* the call,
    because it is embedded in a lambda sent to another smart contract,
    use `get_self_address` instead. The string `entrypoint` is the
    name of a valid entrypoint such that `entrypoint` is not
    `"default"`, or the empty string denoting the `"default"`
    entrypoint (which is the root of the smart contract parameter if
    no `"default"` entrypoint is explicitly defined). If the contract
    does not have the specified entrypoint, the call results in an
    type checking error.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get&#95;self&#95;address : unit -&gt; address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;self&#95;address: (&#95;: unit) =&gt; address
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `get_self_address ()` is the address of the smart
    contract actually executing the call, as a value of type
    `address`. That contract can be different from the one containing
    the call if the call is in a lambda transmitted to another smart
    contract. Therefore, it is assumed that, in general, the type of
    the executing contract is statically unknown, so the return type
    of `get_self_address` is not `'a contract`, but `address`. (See
    `self`.)

</Syntax>

<Syntax syntax="jsligo">

The call `get_self_address()` is the address of the smart
    contract actually executing the call, as a value of type
    `address`. That contract can be different from the one containing
    the call if the call is in a lambda transmitted to another smart
    contract. Therefore, it is assumed that, in general, the type of
    the executing contract is statically unknown, so the return type
    of `get_self_address` is not `'a contract`, but `address`. (See
    `self`.)

</Syntax>


<SyntaxTitle syntax="cameligo">
val address : &#39;a.&#39;a contract -&gt; address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let address: &lt;a&gt;(&#95;: contract&lt;a&gt;) =&gt; address
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `address contract` casts the address of the smart
    contract `contract` into the more general value of type
    `address`.

</Syntax>

<Syntax syntax="jsligo">

The call `address(contract)` casts the address of the smart
    contract `contract` into the more general value of type
    `address`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val implicit&#95;account : key&#95;hash -&gt; unit contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let implicit&#95;account: (&#95;: key&#95;hash) =&gt; contract&lt;unit&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `implicit_account kh` casts the public key hash `kh`
    into the address of its implicit account. Note that addresses of
    implicit accounts always have the type `unit contract`.

</Syntax>

<Syntax syntax="jsligo">

The call `implicit_account(kh)` casts the public key hash `kh`
    into the address of its implicit account. Note that addresses of
    implicit accounts always have the type `contract<unit>`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get&#95;contract&#95;opt : &#39;param.address -&gt; &#39;param contract option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;contract&#95;opt: &lt;param&gt;(&#95;: address) =&gt; option&lt;contract&lt;param&gt;&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `get_contract_opt addr` casts the address `addr` into
    that of a contract address, if such contract exists. The value of
    the call is `None` if no such contract exists, otherwise `Some
    contract`, where `contract` is the contract's address. Note: The
    address of an implicit account has type `unit contract`.

</Syntax>

<Syntax syntax="jsligo">

The call `get_contract_opt(addr)` casts the address `addr` into
    that of a contract address, if such contract exists. The value of
    the call is `None()` if no such contract exists, otherwise `Some
    contract`, where `contract` is the contract's address. Note: The
    address of an implicit account has type `unit contract`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get&#95;contract&#95;with&#95;error : &#39;param.address -&gt; string -&gt; &#39;param contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;contract&#95;with&#95;error: &lt;param&gt;(&#95;: address) =&gt; (&#95;: string) =&gt; contract&lt;param&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `get_contract_with_error addr error` casts the address
    `addr` into that of a contract address, if such contract
    exists. If not, the execution fails with the error message
    `error`.

</Syntax>

<Syntax syntax="jsligo">

The call `get_contract_with_error(addr, error)` casts the address
    `addr` into that of a contract address, if such contract
    exists. If not, the execution fails with the error message
    `error`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get&#95;contract : &#39;param.address -&gt; &#39;param contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;contract: &lt;param&gt;(&#95;: address) =&gt; contract&lt;param&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `get_contract addr` casts the address `addr` into that
    of a smart contract address, if such contract exists. The call
    fails with the message `"bad address for get_contract"` if no
    such smart contract exists. Note: The address of an implicit
    account has type `unit contract`.

</Syntax>

<Syntax syntax="jsligo">

The call `get_contract(addr)` casts the address `addr` into that
    of a smart contract address, if such contract exists. The call
    fails with the message `"bad address for get_contract"` if no
    such smart contract exists. Note: The address of an implicit
    account has type `contract<unit>`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get&#95;entrypoint&#95;opt : &#39;param.string -&gt; address -&gt; &#39;param contract option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;entrypoint&#95;opt: &lt;param&gt;(&#95;: string) =&gt; (&#95;: address) =&gt; option&lt;contract&lt;param&gt;&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `get_entrypoint_opt entrypoint addr` has the same
    behaviour as `get_contract_opt addr`, with the additional
    constraint that the contract must have an entrypoint named
    `entrypoint`. In other words, `get_entrypoint_opt entrypoint addr`
    casts the address `addr` into that of a smart contract
    address, if such contract exists and has an entrypoint named
    `entrypoint`. The value of the call is `None` if no such smart
    contract exists, otherwise `Some contract`, where `contract` is
    the smart contract's address. Note: The address of an implicit
    account has type `unit contract`.

</Syntax>

<Syntax syntax="jsligo">

The call `get_entrypoint_opt(entrypoint, addr)` has the same
    behaviour as `get_contract_opt(addr)`, with the additional
    constraint that the contract must have an entrypoint named
    `entrypoint`. In other words, `get_entrypoint_opt(entrypoint, addr)`
    casts the address `addr` into that of a smart contract
    address, if such contract exists and has an entrypoint named
    `entrypoint`. The value of the call is `None()` if no such smart
    contract exists, otherwise `Some(contract)`, where `contract` is
    the smart contract's address. Note: The address of an implicit
    account has type `contract<unit>`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get&#95;entrypoint : &#39;param.string -&gt; address -&gt; &#39;param contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;entrypoint: &lt;param&gt;(&#95;: string) =&gt; (&#95;: address) =&gt; contract&lt;param&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `get_entrypoint entrypoint addr` casts the address
    `addr` into that of a smart contract address, if such contract
    exists and has an entrypoint named `entrypoint`. If no such smart
    contract exists, the execution fails with the error message
    `"bad address for get_entrypoint"`. Note: The address of an implicit
    account has type `unit contract`.

</Syntax>

<Syntax syntax="jsligo">

The call `get_entrypoint(entrypoint, addr)` casts the address
    `addr` into that of a smart contract address, if such contract
    exists and has an entrypoint named `entrypoint`. If no such smart
    contract exists, the execution fails with the error message
    `"bad address for get_entrypoint"`. Note: The address of an implicit
    account has type `contract<unit>`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val create&#95;contract :
  &#39;param
  &#39;storage.(&#39;param, &#39;storage) entrypoint -&gt; key&#95;hash option -&gt; tez -&gt; &#39;storage -&gt; (operation * address)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let create&#95;contract:
  &lt;param, storage&gt;(&#95;: entrypoint&lt;param, storage&gt;) =&gt; (&#95;: option&lt;key&#95;hash&gt;) =&gt; (&#95;: tez) =&gt; (&#95;: storage) =&gt; [
    operation,
    address
  ]
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `create_contract e d a s` returns a contract creation
    operation (origination) for the entrypoint `e` (as a function)
    with optional delegate `d`, initial amount `a` and initial
    storage `s`, together with the address of the created
    contract. Note that the created contract cannot be called
    immediately afterwards (that is, `get_contract_opt` on that
    address would return `None`), as the origination must be
    performed successfully first, for example by calling a proxy
    contract or itself.

</Syntax>

<Syntax syntax="jsligo">

The call `create_contract(e,d,a,s)` returns a contract creation
    operation (origination) for the entrypoint `e` (as a function)
    with optional delegate `d`, initial amount `a` and initial
    storage `s`, together with the address of the created
    contract. Note that the created contract cannot be called
    immediately afterwards (that is, `get_contract_opt` on that
    address would return `None()`), as the origination must be
    performed successfully first, for example by calling a proxy
    contract or itself.

</Syntax>


<SyntaxTitle syntax="cameligo">
val set&#95;delegate : key&#95;hash option -&gt; operation
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let set&#95;delegate: (&#95;: option&lt;key&#95;hash&gt;) =&gt; operation
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `set_delegate d` evaluates in an operation that sets
    the delegate of the current smart contract to be `d`, where `d` is
    an optional key hash. If `None`, the delegation is withdrawn. If
    the contract has no delegation, then no change occurs. If `d` is
    `Some kh`, where `kh` is the key hash of a registered delegate
    that is not the current delegate of the contract, then this
    operation sets the delegate of the contract to this registered
    delegate. A failure occurs if `kh` is the current delegate of the
    contract or if `kh` is not a registered delegate. However, the
    instruction in itself does not fail; it produces an operation that
    will fail when applied.

</Syntax>

<Syntax syntax="jsligo">

The call `set_delegate(d)` evaluates in an operation that sets
    the delegate of the current smart contract to be `d`, where `d` is
    an optional key hash. If `None()`, the delegation is withdrawn. If
    the contract has no delegation, then no change occurs. If `d` is
    `Some(kh)`, where `kh` is the key hash of a registered delegate
    that is not the current delegate of the contract, then this
    operation sets the delegate of the contract to this registered
    delegate. A failure occurs if `kh` is the current delegate of the
    contract or if `kh` is not a registered delegate. However, the
    instruction in itself does not fail; it produces an operation that
    will fail when applied.

</Syntax>


<SyntaxTitle syntax="cameligo">
val transaction : &#39;param.&#39;param -&gt; tez -&gt; &#39;param contract -&gt; operation
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let transaction: &lt;param&gt;(&#95;: param) =&gt; (&#95;: tez) =&gt; (&#95;: contract&lt;param&gt;) =&gt; operation
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `transaction param amount contract_addr` evaluates in
    an operation that will send the amount `amount` in mutez to the
    contract at the valid address `contract_addr`, with parameter
    `param`. If the contract is an implicit account, the parameter
    must be `unit`.

</Syntax>

<Syntax syntax="jsligo">

The call `transaction(param, amount, contract_addr)` evaluates in
    an operation that will send the amount `amount` in mutez to the
    contract at the valid address `contract_addr`, with parameter
    `param`. If the contract is an implicit account, the parameter
    must be `unit`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val call&#95;view : &#39;param &#39;return.string -&gt; &#39;param -&gt; address -&gt; &#39;return option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let call&#95;view: &lt;param, return&gt;(&#95;: string) =&gt; (&#95;: param) =&gt; (&#95;: address) =&gt; option&lt;return&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `call_view v p a` calls the view `v` with parameter
    `param` at the contract whose address is `a`. The value returned
    is `None` if the view does not exist, or has a different type of
    parameter, or if the contract does not exist at that
    address. Otherwise, it is `Some v`, where `v` is the return value
    of the view. Note: the storage of the view is the same as when the
    execution of the contract calling the view started.

</Syntax>

<Syntax syntax="jsligo">

The call `call_view(v, p, a)` calls the view `v` with parameter
    `param` at the contract whose address is `a`. The value returned
    is `None()` if the view does not exist, or has a different type of
    parameter, or if the contract does not exist at that
    address. Otherwise, it is `Some(v)`, where `v` is the return value
    of the view. Note: the storage of the view is the same as when the
    execution of the contract calling the view started.

</Syntax>


<SyntaxTitle syntax="cameligo">
val create&#95;ticket : &#39;a.&#39;a -&gt; nat -&gt; &#39;a ticket option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let create&#95;ticket: &lt;a&gt;(&#95;: a) =&gt; (&#95;: nat) =&gt; option&lt;ticket&lt;a&gt;&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `create_ticket v a` creates a ticket with value `v` and
    amount `a`. If the creation is a success, the value `Some t` is
    returned, where `t` is the ticket; otherwise, `None` is the
    result. Note: Tickets cannot be duplicated.

</Syntax>

<Syntax syntax="jsligo">

The call `create_ticket(v, a)` creates a ticket with value `v` and
    amount `a`. If the creation is a success, the value `Some(t)` is
    returned, where `t` is the ticket; otherwise, `None()` is the
    result. Note: Tickets cannot be duplicated.

</Syntax>


<SyntaxTitle syntax="cameligo">
val split&#95;ticket : &#39;a.&#39;a ticket -&gt; (nat * nat) -&gt; (&#39;a ticket * &#39;a ticket) option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let split&#95;ticket: &lt;a&gt;(&#95;: ticket&lt;a&gt;) =&gt; (&#95;: [nat, nat]) =&gt; option&lt;[ticket&lt;a&gt;, ticket&lt;a&gt;]&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `split_ticket t (a1, a2)` results in a pair of tickets
    `t1` and `t2` such that the former owns the amount `a1` and the
    later `a2`. More precisely, the value of the call is
    `Some (t1, t2)` because signifying to the callee the failure of
    the splitting is achieved by returning the value `None`.

</Syntax>

<Syntax syntax="jsligo">

The call `split_ticket(t, [a1, a2])` results in a pair of tickets
    `t1` and `t2` such that the former owns the amount `a1` and the
    later `a2`. More precisely, the value of the call is
    `Some([t1, t2])` because signifying to the callee the failure of
    the splitting is achieved by returning the value `None()`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val join&#95;tickets : &#39;a.(&#39;a ticket * &#39;a ticket) -&gt; &#39;a ticket option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let join&#95;tickets: &lt;a&gt;(&#95;: [ticket&lt;a&gt;, ticket&lt;a&gt;]) =&gt; option&lt;ticket&lt;a&gt;&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `join_tickets (t1, t2)` joins the tickets `t1` and
    `t2`, which must have the same type of value.

</Syntax>

<Syntax syntax="jsligo">

The call `join_tickets(t1, t2)` joins the tickets `t1` and
    `t2`, which must have the same type of value.

</Syntax>


<SyntaxTitle syntax="cameligo">
val read&#95;ticket : &#39;a.&#39;a ticket -&gt; (address * &#39;a * nat * &#39;a ticket)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let read&#95;ticket: &lt;a&gt;(&#95;: ticket&lt;a&gt;) =&gt; [[address, [a, nat]], ticket&lt;a&gt;]
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `read_ticket t` returns `t` itself and the contents of
    `t` which is a pair `(address, (value, amount))`, where `address` is
    the address of the smart contract that created it.

</Syntax>

<Syntax syntax="jsligo">

The call `read_ticket(t)` returns `t` itself and the contents of
    `t` which is a pair `[address, [value, amount]]`, where `address` is
    the address of the smart contract that created it.

</Syntax>


<SyntaxTitle syntax="cameligo">
val sapling&#95;empty&#95;state : &#39;sap&#95;t.&#39;sap&#95;t sapling&#95;state
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let sapling&#95;empty&#95;state: &lt;sap&#95;t&gt;sapling&#95;state&lt;sap&#95;t&gt;
</SyntaxTitle>
The evaluation of the constant `sapling_empty_state` is an empty
    sapling state, that is, no one can spend tokens from it.


<SyntaxTitle syntax="cameligo">
val sapling&#95;verify&#95;update :
  &#39;sap&#95;a.&#39;sap&#95;a sapling&#95;transaction -&gt; &#39;sap&#95;a sapling&#95;state -&gt; (bytes * int * &#39;sap&#95;a sapling&#95;state) option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let sapling&#95;verify&#95;update:
  &lt;sap&#95;a&gt;(&#95;: sapling&#95;transaction&lt;sap&#95;a&gt;) =&gt; (&#95;: sapling&#95;state&lt;sap&#95;a&gt;) =&gt; option&lt;[bytes, [int, sapling&#95;state&lt;sap&#95;a&gt;]]&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `sapling_verify_update trans state`, where the
    transaction `trans` can be applied to the state `state`, returns
    `Some (data, (delta, new_state))`, where `data` is the bound data
    (as bytes), `delta` is the difference between the outputs and the
    inputs of the transaction, and `new_state` is the updated
    state.

</Syntax>

<Syntax syntax="jsligo">

The call `sapling_verify_update(trans, state)`, where the
    transaction `trans` can be applied to the state `state`, returns
    `Some ([data, [delta, new_state]])`, where `data` is the bound data
    (as bytes), `delta` is the difference between the outputs and the
    inputs of the transaction, and `new_state` is the updated
    state.

</Syntax>


<SyntaxTitle syntax="cameligo">
val emit : &#39;event&#95;type.string -&gt; &#39;event&#95;type -&gt; operation
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let emit: &lt;event&#95;type&gt;(&#95;: string) =&gt; (&#95;: event&#95;type) =&gt; operation
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `emit event_tag event_type` evaluates in an operation
    that will write an event into the transaction receipt after the
    successful execution of this contract. The event is annotated by
    the string `event_tag` if it is not empty. The argument
    `event_type` is used only to specify the type of data attachment.

</Syntax>

<Syntax syntax="jsligo">

The call `emit event_tag(event_type)` evaluates in an operation
    that will write an event into the transaction receipt after the
    successful execution of this contract. The event is annotated by
    the string `event_tag` if it is not empty. The argument
    `event_type` is used only to specify the type of data attachment.

</Syntax>


<SyntaxTitle syntax="cameligo">
val open&#95;chest : chest&#95;key -&gt; chest -&gt; nat -&gt; bytes option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let open&#95;chest: (&#95;: chest&#95;key) =&gt; (&#95;: chest) =&gt; (&#95;: nat) =&gt; option&lt;bytes&gt;
</SyntaxTitle>
The function [open_chest] opens a timelocked chest given its key
    and the time. The result is a byte option depending if the opening
    is correct or not.


<SyntaxTitle syntax="cameligo">
val get&#95;balance : unit -&gt; tez
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;balance: (&#95;: unit) =&gt; tez
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `get_balance ()` returns the balance in mutez of the
    account associated to the currently executed smart contract,
    including any mutez added by the calling transaction.

</Syntax>

<Syntax syntax="jsligo">

The call `get_balance()` returns the balance in mutez of the
    account associated to the currently executed smart contract,
    including any mutez added by the calling transaction.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get&#95;amount : unit -&gt; tez
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;amount: (&#95;: unit) =&gt; tez
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `get_amount ()` returns the amount in mutez of the
    current transaction.

</Syntax>

<Syntax syntax="jsligo">

The call `get_amount()` returns the amount in mutez of the
    current transaction.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get&#95;now : unit -&gt; timestamp
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;now: (&#95;: unit) =&gt; timestamp
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `get_now ()` returns the minimal injection time for the
    current block, namely the block whose application triggered this
    execution. The minimal injection time constitutes an estimate of
    the moment when the current block is injected, hence the name
    "now".

</Syntax>

<Syntax syntax="jsligo">

The call `get_now()` returns the minimal injection time for the
    current block, namely the block whose application triggered this
    execution. The minimal injection time constitutes an estimate of
    the moment when the current block is injected, hence the name
    "now".

</Syntax>


<SyntaxTitle syntax="cameligo">
val get&#95;min&#95;block&#95;time : unit -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;min&#95;block&#95;time: (&#95;: unit) =&gt; nat
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `get_min_block_time ()` returns the minimal delay
    between two consecutive blocks in the chain.

</Syntax>

<Syntax syntax="jsligo">

The call `get_min_block_time()` returns the minimal delay
    between two consecutive blocks in the chain.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get&#95;level : unit -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;level: (&#95;: unit) =&gt; nat
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `get_level ()` returns the current block level.

</Syntax>

<Syntax syntax="jsligo">

The call `get_level()` returns the current block level.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get&#95;chain&#95;id : unit -&gt; chain&#95;id
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;chain&#95;id: (&#95;: unit) =&gt; chain&#95;id
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `get_chain_id ()` returns the identifier of the chain
    on which the smart contract is executed.

</Syntax>

<Syntax syntax="jsligo">

The call `get_chain_id ()` returns the identifier of the chain
    on which the smart contract is executed.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get&#95;total&#95;voting&#95;power : unit -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;total&#95;voting&#95;power: (&#95;: unit) =&gt; nat
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `get_total_voting_power ()` returns the total voting
    power of all contracts. The total voting power coincides with the
    sum of the stake of every contract in the voting listings. The
    voting listings is calculated at the beginning of every voting
    period.

</Syntax>

<Syntax syntax="jsligo">

The call `get_total_voting_power()` returns the total voting
    power of all contracts. The total voting power coincides with the
    sum of the stake of every contract in the voting listings. The
    voting listings is calculated at the beginning of every voting
    period.

</Syntax>


<SyntaxTitle syntax="cameligo">
val voting&#95;power : key&#95;hash -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let voting&#95;power: (&#95;: key&#95;hash) =&gt; nat
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `voting_power contract_kh` returns the voting power of
    a given contract specified by the key hash `contract_kh`. This
    voting power coincides with the weight of the contract in the
    voting listings (that is, the stake) which is calculated at the
    beginning of every voting period.

</Syntax>

<Syntax syntax="jsligo">

The call `voting_power(contract_kh)` returns the voting power of
    a given contract specified by the key hash `contract_kh`. This
    voting power coincides with the weight of the contract in the
    voting listings (that is, the stake) which is calculated at the
    beginning of every voting period.

</Syntax>


<SyntaxTitle syntax="cameligo">
val never : &#39;a.never -&gt; &#39;a
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let never: &lt;a&gt;(&#95;: never) =&gt; a
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `never n` is never meant to be executed, as the type
    `never` is inhabited, but to instruct the typechecker that a
    branch in the control flow, for example, in a pattern matching, is
    dead.

</Syntax>

<Syntax syntax="jsligo">

The call `never(n)` is never meant to be executed, as the type
    `never` is inhabited, but to instruct the typechecker that a
    branch in the control flow, for example, in a pattern matching, is
    dead.

</Syntax>


<SyntaxTitle syntax="cameligo">
val pairing&#95;check : (bls12&#95;381&#95;g1 * bls12&#95;381&#95;g2) list -&gt; bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let pairing&#95;check: (&#95;: list&lt;[bls12&#95;381&#95;g1, bls12&#95;381&#95;g2]&gt;) =&gt; bool
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `pairing_check pairings` verifies that the product of
    pairings of the given list of points `pairings` is equal to 1 in
    the field Fq12. It evaluates in `true` if the list is empty. This
    function can be used to verify if two pairings P1 and P2 are equal
    by verifying P1 * P2^(-1) = 1.

</Syntax>

<Syntax syntax="jsligo">

The call `pairing_check(pairings)` verifies that the product of
    pairings of the given list of points `pairings` is equal to 1 in
    the field Fq12. It evaluates in `true` if the list is empty. This
    function can be used to verify if two pairings P1 and P2 are equal
    by verifying P1 * P2^(-1) = 1.

</Syntax>


<SyntaxTitle syntax="cameligo">
val constant : &#39;a.string -&gt; &#39;a
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let constant: &lt;a&gt;(&#95;: string) =&gt; a
</SyntaxTitle>
<Syntax syntax="cameligo">

The call to `constant hash` returns the value stored on-chain
    whose hash value is `hash` (global constants). This call can fail
    when the contract is originated if the hash is invalid or the
    expansion of the global constant is ill-typed, or too large (gas
    consumption).

</Syntax>

<Syntax syntax="cameligo">

The call to `constant(hash)` returns the value stored on-chain
    whose hash value is `hash` (global constants). This call can fail
    when the contract is originated if the hash is invalid or the
    expansion of the global constant is ill-typed, or too large (gas
    consumption).

</Syntax>
