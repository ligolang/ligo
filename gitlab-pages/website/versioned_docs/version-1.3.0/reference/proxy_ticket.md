---
id: proxy-ticket-reference
title: Proxy_ticket
description: Helper functions for testing tickets.
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

> Note: This module/namespace exists inside the Test module/namespace, to use
> functions from this module prefix it with Test. e.g. `Test.Proxy_ticket.init_transfer`

Helper functions for working with tickets in the LIGO Testing framework.

<SyntaxTitle syntax="cameligo">
type 'a proxy_address = (('a * nat) * address , unit) typed_address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
type proxy_address&lt;T&gt; =  typed_address&lt;[[T, nat], address], unit&gt;
</SyntaxTitle>

Typed address of the proxy contract.

<SyntaxTitle syntax="cameligo">
val init_transfer : ('a ticket -> 'param) -> 'a proxy_address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let init_transfer : &lt;T, P&gt;(mk_param: (t: ticket&lt;T&gt;) => P) => proxy_address&lt;T&gt;
</SyntaxTitle>

Takes a function that given a ticket creates the parameter for calling/transferring
to a contract (param creator) and returns a typed address (`proxy_address`) of the proxy contract.

This function originates a proxy contract, the actual transfer of the ticket needs to
go through this proxy contract (done via the `transfer` function below).

<SyntaxTitle syntax="cameligo">
val transfer : 'a proxy_address -> (('a * nat) * address) -> test_exec_result
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let transfer : &lt;T&gt;(p: proxy_address&lt;T&gt;, ti: [[T, nat], address]) => test_exec_result
</SyntaxTitle>

Takes the `proxy_address` (typed address of the proxy contract), and the ticket
information along with the destination address (The contract to which the ticket needs to be transferred).

This function makes the actual transfer of the ticket to the destination contract/address.

<SyntaxTitle syntax="cameligo">
val originate : ('a * nat) -> ('a ticket -> 'storage) -> ('param -> 'storage -> operation list * 'storage) -> address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let originate : &lt;T, P, S&gt;(ti: [T, nat], mk_storage: (t: ticket&lt;T&gt;) => S, c: (p: P, s: S) => [list&lt;operation&gt;, S]) => address
</SyntaxTitle>

Takes ticket information along with a function that when takes a ticket and returns
the storage of the contract to be originated and the actual contract to be originated
and returns the address of the originated contract.

This function internally originated the contract via a proxy contract.

<SyntaxTitle syntax="cameligo">
val originate_uncurried : ('a * nat) -> ('a ticket -> 'storage) -> ('param * 'storage -> operation list * 'storage) -> address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let originate_uncurried : &lt;T, P, S&gt;(ti: [T, nat], mk_storage: (t: ticket&lt;T&gt;) => S, c: (ps: [P, S]) => [list&lt;operation&gt;, S]) => address
</SyntaxTitle>

Same as `originate` described above, but works with un-curried contracts.
