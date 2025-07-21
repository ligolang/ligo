---
id: tezos.operation-reference
title: operation
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



<SyntaxTitle syntax="cameligo">
val create_contract :
  &#39;param
  &#39;storage.(&#39;param, &#39;storage) entrypoint -&gt; key_hash option -&gt; tez -&gt; &#39;storage -&gt; (operation * address)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
create_contract:
  &lt;param, storage&gt;(entrypoint: entrypoint&lt;param, storage&gt;,
  delegate: option&lt;key_hash&gt;, amount: tez, _: storage) =&gt; [
    operation,
    address
  ]
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `create_contract e d a s` returns a contract creation
operation (origination) for the entrypoint `e` (as a function) with
optional delegate `d`, initial amount `a` and initial storage `s`,
together with the address of the created contract. Note that the
created contract cannot be called immediately afterwards (that is,
`get_contract_opt` on that address would return `None`), as the
origination must be performed successfully first, for example by
calling a proxy contract or itself.

</Syntax>

<Syntax syntax="jsligo">

The call `create_contract(e,d,a,s)` returns a contract
creation operation (origination) for the entrypoint `e` (as a
function) with optional delegate `d`, initial amount `a` and
initial storage `s`, together with the address of the created
contract. Note that the created contract cannot be called
immediately afterwards (that is, `get_contract_opt` on that
address would return `["None" as "None"]`), as the origination
must be performed successfully first, for example by calling a
proxy contract or itself.

</Syntax>


<SyntaxTitle syntax="cameligo">
val set_delegate : key_hash option -&gt; operation
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
set_delegate: (delegate: option&lt;key_hash&gt;) =&gt; operation
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

The call `set_delegate(d)` evaluates in an operation that sets the
delegate of the current smart contract to be `d`, where `d` is an
optional key hash. If `["None" as "None"]`, the delegation is
withdrawn. If the contract has no delegation, then no change
occurs. If `d` is `["Some" as "Some", kh]`, where `kh` is the key hash
of a registered delegate that is not the current delegate of the
contract, then this operation sets the delegate of the contract to
this registered delegate. A failure occurs if `kh` is the current
delegate of the contract or if `kh` is not a registered
delegate. However, the instruction in itself does not fail; it
produces an operation that will fail when applied.

</Syntax>


<SyntaxTitle syntax="cameligo">
val transaction : &#39;param.&#39;param -&gt; tez -&gt; &#39;param contract -&gt; operation
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
transaction: &lt;param&gt;(_: param, amount: tez, contract_addr: contract&lt;param&gt;) =&gt; operation
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `transaction param amount contract_addr` evaluates in an
operation that will send the amount `amount` in mutez to the contract
at the valid address `contract_addr`, with parameter `param`. If the
contract is an implicit account, the parameter must be `unit`.

</Syntax>

<Syntax syntax="jsligo">

The call `transaction(param, amount, contract_addr)` evaluates in an
operation that will send the amount `amount` in mutez to the contract
at the valid address `contract_addr`, with parameter `param`. If the
contract is an implicit account, the parameter must be `unit`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val emit : &#39;event_type.string -&gt; &#39;event_type -&gt; operation
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
emit: &lt;event_type&gt;(even_tag: string, _: event_type) =&gt; operation
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `emit event_tag event_type` evaluates in an operation that
will write an event into the transaction receipt after the successful
execution of this contract. The event is annotated by the string
`event_tag` if it is not empty. The argument `event_type` is used only
to specify the type of data attachment.

</Syntax>

<Syntax syntax="jsligo">

The call `emit event_tag(event_type)` evaluates in an operation that
will write an event into the transaction receipt after the successful
execution of this contract. The event is annotated by the string
`event_tag` if it is not empty. The argument `event_type` is used only
to specify the type of data attachment.

</Syntax>
